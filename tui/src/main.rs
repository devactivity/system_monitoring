use crossterm::{
    event::{self, Event, KeyCode},
    terminal::{disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Style, Stylize},
    symbols,
    text::Text,
    widgets::{Block, Borders, Paragraph, RenderDirection, Sparkline},
    Terminal,
};
use serde::Deserialize;
use std::{
    collections::VecDeque,
    fmt::{self, format},
    io::{self, BufRead},
    time::Duration,
};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};

#[derive(Clone, Default, Copy, Debug, Deserialize)]
struct MetricEntry {
    cpu_usage: f32,
    mem_usage: f32,
    disk_read: f64,
    disk_write: f64,
}

impl fmt::Display for MetricEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "CPU Usage: {:.2}%, Mem Usage: {:.2}%, Disk Read: {:.2} MB, Disk Write: {:.2} MB",
            self.cpu_usage, self.mem_usage, self.disk_read, self.disk_write,
        )
    }
}

struct MetricsHistory {
    cpu_usage: VecDeque<f32>,
    mem_usage: VecDeque<f32>,
}

impl MetricsHistory {
    fn new() -> Self {
        MetricsHistory {
            cpu_usage: VecDeque::new(),
            mem_usage: VecDeque::new(),
        }
    }

    fn add_cpu(&mut self, value: f32) {
        self.cpu_usage.push_back(value);
    }

    fn add_mem(&mut self, value: f32) {
        self.mem_usage.push_back(value);
    }
}

#[tokio::main]
async fn main() -> Result<(), io::Error> {
    enable_raw_mode()?;
    let backend = CrosstermBackend::new(io::stdout());
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut should_quit = false;
    let mut error_message = None;
    let mut metrics_history = MetricsHistory::new();

    while !should_quit {
        let new_metrics = fetch_metrics().await;

        if new_metrics.cpu_usage > 0.0 || new_metrics.mem_usage > 0.0 {
            metrics_history.add_cpu(new_metrics.cpu_usage);
            metrics_history.add_mem(new_metrics.mem_usage);
            error_message = None;
        } else {
            error_message = Some("failed to fetch metrics".to_string());
        }

        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(1)
                .constraints([
                    Constraint::Percentage(45),
                    Constraint::Percentage(45),
                    Constraint::Percentage(10),
                ])
                .split(f.area());

            // CPU sparkline
            let cpu_data =
                prepare_sparkline_data(&metrics_history.cpu_usage, chunks[0].width as usize);
            let cpu_spark = Sparkline::default()
                .block(Block::default().title("CPU Usage").borders(Borders::ALL))
                .data(cpu_data.as_slice())
                .max(1000) // 1000 * 100.0
                .direction(RenderDirection::LeftToRight)
                .style(Style::default().fg(Color::Green))
                .absent_value_style(Style::default().fg(Color::Red))
                .absent_value_symbol(symbols::shade::FULL);

            // memory sparkline
            let mem_data =
                prepare_sparkline_data(&metrics_history.mem_usage, chunks[1].width as usize);

            let mem_spark = Sparkline::default()
                .block(Block::default().title("Mem Usage").borders(Borders::ALL))
                .data(mem_data.as_slice())
                .max(1000) // 1000 * 100.0
                .direction(RenderDirection::LeftToRight)
                .style(Style::default().fg(Color::Blue))
                .absent_value_style(Style::default().fg(Color::Red))
                .absent_value_symbol(symbols::shade::FULL);

            let status_text = create_status_text(&new_metrics, &error_message);

            f.render_widget(cpu_spark, chunks[0]);
            f.render_widget(mem_spark, chunks[1]);
            f.render_widget(status_text, chunks[2]);
        })?;

        if event::poll(Duration::from_millis(50))? {
            if let Event::Key(key) = event::read()? {
                if key.code == KeyCode::Char('q') {
                    should_quit = true;
                }
            }
        }

        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    disable_raw_mode()?;
    terminal.clear()?;

    Ok(())
}

fn prepare_sparkline_data(history: &VecDeque<f32>, width: usize) -> Vec<u64> {
    let mut data: Vec<u64> = history
        .iter()
        .rev()
        .take(width)
        .map(|&v| (v * 10.0) as u64)
        .collect();

    data.reverse();

    if data.is_empty() {
        vec![0; width]
    } else {
        let last = *data.last().unwrap();
        let mut padded = data.clone();
        padded.resize(width, last);
        padded
    }
}

fn create_status_text<'a>(metric: &'a MetricEntry, error: &'a Option<String>) -> Paragraph<'a> {
    let status = match error {
        Some(msg) => Text::from(format!("Status: {msg}")).red(),
        None => Text::from(format!(
            "Disk R/W: {:.1}MB/s | {:.1}MB/s",
            metric.disk_read, metric.disk_write,
        ))
        .green(),
    };

    Paragraph::new(status).block(Block::default().borders(Borders::ALL).title("Disk I/O"))
}

async fn fetch_metrics() -> MetricEntry {
    let mut stream = match TcpStream::connect("127.0.0.1:9000").await {
        Ok(s) => s,
        Err(_) => return MetricEntry::default(),
    };

    if let Err(_) = stream.write_all(b"get_metrics").await {
        return MetricEntry::default();
    }

    // use dynamic buffer
    let mut buffer = Vec::new();
    let mut temp_buf = [0_u8; 1024];

    // read chunks
    loop {
        match stream.read(&mut temp_buf).await {
            Ok(0) => break, // connection closed
            Ok(n) => {
                buffer.extend_from_slice(&temp_buf[..n]);
                if buffer.contains(&b']') || (buffer.contains(&b'{') && buffer.contains(&b'}')) {
                    break;
                }
                if buffer.len() > 1_000_000 {
                    break;
                }
            }
            Err(_) => return MetricEntry::default(),
        }
    }

    if let Ok(s) = std::str::from_utf8(&buffer) {
        if s.contains("cpu_usage") {
            if let Some(start) = s.find('{') {
                if let Some(end) = s[start..].find('}') {
                    let obj_str = &s[start..=start + end];

                    if let Ok(entry) = serde_json::from_str::<MetricEntry>(obj_str) {
                        return entry;
                    }

                    // manual extraction
                    let cpu = extract_value(obj_str, "cpu_usage").unwrap_or(0.0);
                    let mem = extract_value(obj_str, "mem_usage").unwrap_or(0.0);
                    let disk_r = extract_value(obj_str, "disk_read").unwrap_or(0.0);
                    let disk_w = extract_value(obj_str, "disk_write").unwrap_or(0.0);

                    if cpu > 0.0 || mem > 0.0 {
                        return MetricEntry {
                            cpu_usage: cpu,
                            mem_usage: mem,
                            disk_read: disk_r as f64,
                            disk_write: disk_w as f64,
                        };
                    }
                }
            }
        }
    }

    MetricEntry::default()
}

fn extract_value(s: &str, key: &str) -> Option<f32> {
    let pattern = format!("\"{}\":\\s*([0-9.e]+)", key);
    let re = regex::Regex::new(&pattern).ok()?;
    let capture = re.captures(s)?;
    let val_str = capture.get(1)?.as_str();

    val_str.parse::<f32>().ok()
}

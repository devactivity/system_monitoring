use serde::Serialize;
use std::{fs::File, io::Read, time::Duration};
use tokio::{io::AsyncWriteExt, net::TcpStream};

#[derive(Serialize)]
struct Metrics {
    cpu_usage: f32,
    mem_usage: f32,
    disk_read: f64,
    disk_write: f64,
}

#[tokio::main]
async fn main() {
    let mut last_cpu = get_cpu_stats();
    let mut last_disk = get_disk_stats();

    loop {
        let cpu = get_cpu_stats();
        let mem = get_mem_usage();
        let disk = get_disk_stats();

        let cpu_usage = calculate_cpu_usage(&last_cpu, &cpu);
        let (disk_read, disk_write) = calculate_disk_usage(&last_disk, &disk);

        let metrics = Metrics {
            cpu_usage,
            mem_usage: mem,
            disk_read,
            disk_write,
        };

        send_metrics(metrics).await;
        last_cpu = cpu;
        last_disk = disk;

        tokio::time::sleep(Duration::from_secs(1)).await;
    }
}

fn get_cpu_stats() -> (u64, u64, u64, u64) {
    let mut file = File::open("/proc/stat").unwrap();
    let mut buffer = String::new();

    file.read_to_string(&mut buffer).unwrap();

    let line = buffer.lines().next().unwrap();
    let parts: Vec<&str> = line.split_whitespace().collect();

    (
        parts[1].parse().unwrap(), // usertime
        parts[2].parse().unwrap(), // nicetime
        parts[3].parse().unwrap(), // systemtime
        parts[4].parse().unwrap(), // idletime
    )
}

fn calculate_cpu_usage(last: &(u64, u64, u64, u64), current: &(u64, u64, u64, u64)) -> f32 {
    let total_last = last.0 + last.1 + last.2 + last.3;
    let total_current = current.0 + current.1 + current.2 + current.3;

    let idle_last = last.3;
    let idle_current = current.3;

    let total_diff = total_current - total_last;
    let idle_diff = idle_current - idle_last;

    (1.0 - (idle_diff as f32 / total_diff as f32)) * 100.0
}

fn get_mem_usage() -> f32 {
    let mut file = File::open("/proc/meminfo").unwrap();
    let mut buffer = String::new();

    file.read_to_string(&mut buffer).unwrap();

    let total = buffer
        .lines()
        .find(|l| l.starts_with("MemTotal"))
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse::<u64>()
        .unwrap();
    let free = buffer
        .lines()
        .find(|l| l.starts_with("MemAvailable"))
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse::<u64>()
        .unwrap();

    ((total - free) as f32 / total as f32) * 100.0
}

#[link(name = "diskio", kind = "dylib")]
extern "C" {
    fn get_disk_io(read: *mut f64, write: *mut f64);
}

fn get_disk_stats() -> (f64, f64) {
    let mut read = 0.0;
    let mut write = 0.0;

    unsafe { get_disk_io(&mut read, &mut write) };

    (read, write)
}

fn calculate_disk_usage(last: &(f64, f64), current: &(f64, f64)) -> (f64, f64) {
    (current.0 - last.0, current.1 - last.1)
}

async fn send_metrics(metrics: Metrics) {
    let json = serde_json::to_string(&metrics).unwrap();

    if let Ok(mut stream) = TcpStream::connect("127.0.0.1:9000").await {
        stream.write_all(json.as_bytes()).await.unwrap();
    }
}

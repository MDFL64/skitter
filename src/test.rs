use std::{
    collections::VecDeque,
    error::Error,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process::Command,
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

struct SharedTestState {
    count_success: i32,
    time_skitter_total: Duration,
    tests: VecDeque<TestInfo>,
}

pub fn test(dir_name: &Path, global_args: Vec<OsString>) -> ! {
    use colored::Colorize;

    println!();
    println!("base skitter overhead = {:?}", get_base_overhead());
    let worker_count = get_worker_count();
    println!("using {} workers", worker_count);
    println!();

    let mut test_files = Vec::new();
    gather_tests(dir_name, &mut test_files);
    test_files.sort();

    let count_total = test_files.len();

    let global_test_state = Arc::new(Mutex::new(SharedTestState {
        count_success: 0,
        time_skitter_total: Duration::ZERO,
        tests: test_files.into(),
    }));

    let workers: Vec<_> = (0..worker_count)
        .map(|worker_n| {
            let global_args = global_args.clone();
            let global_test_state = global_test_state.clone();

            std::thread::spawn(move || {
                let bin_name = format!("/tmp/skitter_test_{}", worker_n);
                let bin_path = Path::new(&bin_name);

                loop {
                    let file = {
                        let mut state = global_test_state.lock().expect("test worker panicked");
                        state.tests.pop_front()
                    };

                    if let Some(file) = file {
                        let res = run_test(&file, bin_path, &global_args);
                        let res_str: String = match res {
                            Err(msg) => format!("{}", format!("FAIL: {}", msg).red()),
                            Ok(res) => {
                                {
                                    let mut state =
                                        global_test_state.lock().expect("test worker panicked");
                                    state.count_success += 1;
                                    state.time_skitter_total += res.time_skitter;
                                }

                                let percent = format!("{:.1}%", res.fraction * 100.0);

                                let speedup_str = if res.fraction < 0.1 {
                                    percent.green()
                                } else if res.fraction < 1.0 {
                                    percent.yellow()
                                } else {
                                    percent.red()
                                };

                                format!(
                                    "{} {} ({:?} / {:?})",
                                    "GOOD:".green(),
                                    speedup_str,
                                    res.time_skitter,
                                    res.time_rustc
                                )
                            }
                        };

                        println!("{:40} {}", file.file.to_str().unwrap(), res_str);
                    } else {
                        break;
                    }
                }
            })
        })
        .collect();

    for worker in workers {
        worker.join().expect("test worker panicked");
    }

    /*let bin_name = Path::new("/tmp/skitter_test_1");
    let mut count_success = 0;
    let mut time_skitter_total: Duration = Default::default();
    for file in test_files {
        let res = run_test(&file, bin_name, global_args);
        let res_str: String = match res {
            Err(msg) => format!("{}", format!("FAIL: {}", msg).red()),
            Ok(res) => {
                count_success += 1;

                let percent = format!("{:.1}%", res.fraction * 100.0);

                let speedup_str = if res.fraction < 0.1 {
                    percent.green()
                } else if res.fraction < 1.0 {
                    percent.yellow()
                } else {
                    percent.red()
                };

                time_skitter_total += res.time_skitter;

                format!(
                    "{} {} ({:?} / {:?})",
                    "GOOD:".green(),
                    speedup_str,
                    res.time_skitter,
                    res.time_rustc
                )
            }
        };

        println!("{:40} {}", file.file.to_str().unwrap(), res_str);
    }*/

    {
        let state = global_test_state.lock().expect("test worker panicked");

        println!();
        println!("{} / {} tests passed", state.count_success, count_total);
        println!("skitter took {:?} total", state.time_skitter_total);
        println!();
    }

    std::process::exit(0)
}

#[derive(Ord, Eq, PartialEq, PartialOrd)]
struct TestInfo {
    file: PathBuf,
    args: Vec<OsString>,
}

fn gather_tests(dir_name: &Path, files: &mut Vec<TestInfo>) {
    let dir_path = Path::new(dir_name);

    let args: Vec<OsString> = if let Result::Ok(data) = std::fs::read(dir_path.join("_args")) {
        let arg_data = std::str::from_utf8(&data).expect("bad args");
        arg_data
            .split(' ')
            .filter_map(|a| {
                let a = a.trim();
                if a.len() == 0 {
                    None
                } else {
                    Some(OsString::from(a))
                }
            })
            .collect()
    } else {
        vec![]
    };

    let read_dir = std::fs::read_dir(dir_name).expect("failed to read test directory");
    for entry in read_dir {
        if let Ok(entry) = entry {
            if let Some(file_name) = entry.file_name().to_str() {
                if file_name.starts_with('_') {
                    continue;
                }
                if let Ok(file_ty) = entry.file_type() {
                    if file_ty.is_dir() {
                        let sub_dir = dir_path.join(file_name);
                        gather_tests(&sub_dir, files);
                    } else if file_ty.is_file() {
                        if file_name.ends_with(".rs") {
                            let file = dir_path.join(file_name);
                            files.push(TestInfo {
                                file,
                                args: args.clone(),
                            });
                        }
                    }
                }
            }
        }
    }
}

struct TestResult {
    time_rustc_compile: Duration,
    time_rustc_exec: Duration,
    time_rustc: Duration,
    time_skitter: Duration,
    fraction: f64,
}

const ERROR_CHARS: usize = 80;

fn run_test(
    test_info: &TestInfo,
    bin_name: &Path,
    global_args: &[OsString],
) -> Result<TestResult, String> {
    // Rust compile
    let t = Instant::now();
    {
        let fail = || Err(String::from("rustc compile failed"));

        let cmd_res = Command::new("rustc")
            .arg(&test_info.file)
            .arg("-o")
            .arg(bin_name)
            .arg("-C")
            .arg("overflow-checks=off")
            .output();

        if let Ok(cmd_res) = cmd_res {
            if !cmd_res.status.success() {
                if let Ok(output) = std::str::from_utf8(&cmd_res.stderr) {
                    println!("{}", output);
                }
                return fail();
            }
        } else {
            return fail();
        }
    }
    let time_rustc_compile = t.elapsed();

    // Rust execute
    let t = Instant::now();
    let rustc_out = {
        let fail = || Err(String::from("rustc exec failed"));

        let cmd_res = Command::new(bin_name).output();
        if let Ok(cmd_res) = cmd_res {
            if !cmd_res.status.success() {
                return fail();
            } else {
                cmd_res.stdout
            }
        } else {
            return fail();
        }
    };
    let time_rustc_exec = t.elapsed();

    // Skitter interpreter
    let (skitter_out, time_skitter) = {
        let fail = || Err(String::from("skitter failed"));

        let program = std::env::current_exe().expect("failed to get skitter path");

        let mut args = Vec::new();
        args.push(test_info.file.as_os_str());

        for arg in global_args {
            args.push(arg);
        }

        for arg in test_info.args.iter() {
            args.push(arg);
        }

        let cmd_res = time_command(&program, &args);

        if let Ok(cmd_res) = cmd_res {
            if !cmd_res.success {
                let skitter_err =
                    std::str::from_utf8(&cmd_res.stderr).expect("failed to read stderr as utf8");
                let first_line = skitter_err.lines().nth(0).unwrap_or_else(|| "");

                let first_line = if first_line.len() > ERROR_CHARS {
                    &first_line[..ERROR_CHARS]
                } else {
                    first_line
                };

                return Err(format!("skitter failed ( {} )", first_line));
            } else {
                (cmd_res.stdout, cmd_res.time)
            }
        } else {
            return fail();
        }
    };

    if rustc_out != skitter_out {
        Err("output mismatch".into())
    } else {
        let time_rustc = time_rustc_compile + time_rustc_exec;
        Ok(TestResult {
            time_rustc_compile,
            time_rustc_exec,
            time_rustc,
            time_skitter,
            fraction: time_skitter.as_secs_f64() / time_rustc.as_secs_f64(),
        })
    }
}

struct TimeResult {
    success: bool,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    time: Duration,
}

fn time_command(cmd_name: &Path, args: &[&OsStr]) -> Result<TimeResult, Box<dyn Error>> {
    let mut cmd = Command::new(cmd_name);
    cmd.args(args);

    let t = std::time::Instant::now();
    let output = cmd.output()?;
    let time = t.elapsed();

    let success = output.status.success();

    Ok(TimeResult {
        success,
        stdout: output.stdout,
        stderr: output.stderr,
        time,
    })
}

fn get_base_overhead() -> Duration {
    let program = std::env::current_exe().expect("failed to get skitter path");

    let n = 5;

    let sum: Duration = (0..n)
        .map(|_| time_command(&program, &[]).unwrap().time)
        .sum();
    sum / n
}

/// NOTE: Individual tests may experience some slowdown with many workers.
/// Both rustc and skitter are affected, so results aren't completely skewed,
/// but you may want to use a single worker for more accurate timing.
fn get_worker_count() -> usize {
    if let Ok(val) = std::env::var("TEST_WORKERS") {
        if let Ok(res) = val.parse::<usize>() {
            return res;
        }
    }

    let res = if let Ok(n) = std::thread::available_parallelism() {
        n.get()
    } else {
        1
    };
    res
}

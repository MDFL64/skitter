use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

pub fn test(dir_name: &str) -> ! {
    use colored::Colorize;

    let mut test_files = Vec::new();
    gather_tests(dir_name, &mut test_files);
    test_files.sort();

    let bin_name = Path::new("/tmp/skitter_test_1");
    let mut count_success = 0;
    let count_total = test_files.len();
    let mut time_skitter_total: Duration = Default::default();
    for file in test_files {
        let res = run_test(&file, bin_name);
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
    }

    println!("=> {} / {} tests passed", count_success, count_total);
    println!("=> skitter took {:?} total", time_skitter_total);

    std::process::exit(0)
}

#[derive(Ord, Eq, PartialEq, PartialOrd)]
struct TestInfo {
    file: PathBuf,
    args: Vec<String>,
}

fn gather_tests(dir_name: &str, files: &mut Vec<TestInfo>) {
    let dir_path = Path::new(dir_name);

    let args: Vec<String> = if let Result::Ok(data) = std::fs::read(dir_path.join("_args")) {
        let arg_data = std::str::from_utf8(&data).expect("bad args");
        arg_data
            .split(' ')
            .filter_map(|a| {
                let a = a.trim();
                if a.len() == 0 {
                    None
                } else {
                    Some(a.to_owned())
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
                        if let Some(sub_dir) = sub_dir.to_str() {
                            gather_tests(sub_dir, files);
                        }
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

fn run_test(test_info: &TestInfo, bin_name: &Path) -> Result<TestResult, String> {
    use std::process::Command;

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
    let t = Instant::now();
    let skitter_out = {
        let fail = || Err(String::from("skitter failed"));

        let program = std::env::current_exe().expect("failed to get skitter path");

        let mut cmd = Command::new(program);
        cmd.arg(&test_info.file);

        for arg in test_info.args.iter() {
            cmd.arg(arg);
        }

        let cmd_res = cmd.output();

        if let Ok(cmd_res) = cmd_res {
            if !cmd_res.status.success() {
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
                cmd_res.stdout
            }
        } else {
            return fail();
        }
    };
    let time_skitter = t.elapsed();

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

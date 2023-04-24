use std::{path::{Path, PathBuf}, time::{Duration, Instant}};

pub fn test(dir_name: &str) -> ! {
    use colored::Colorize;

    let mut test_files = Vec::new();
    gather_tests(dir_name, &mut test_files);
    test_files.sort();

    let bin_name = Path::new("/tmp/skitter_test_1");
    let mut count_success = 0;
    let count_total = test_files.len();
    for file in test_files {
        let res = run_test(&file, bin_name);
        let res_str: String = match res {
            Err(msg) => format!("{}", format!("FAIL: {}", msg).red()),
            Ok(res) => {
                count_success += 1;
                
                let percent = format!("{:.1}%", res.fraction*100.0);

                let speedup_str = if res.fraction < 1.0 {
                    percent.green()
                } else {
                    percent.yellow()
                };

                format!("{} {} ({:?} / {:?})", "GOOD:".green(), speedup_str, res.time_skitter, res.time_rustc)
            }
        };

        println!("{:40} {}", file.to_str().unwrap(), res_str);
        //println!();
    }

    println!("=> {} / {} tests passed", count_success, count_total);

    std::process::exit(0)
}

fn gather_tests(dir_name: &str, files: &mut Vec<PathBuf>) {
    let dir_path = Path::new(dir_name);
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
                            files.push(file);
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

fn run_test(file_name: &Path, bin_name: &Path) -> Result<TestResult, String> {
    use std::process::Command;

    // Rust compile
    let t = Instant::now();
    {
        let fail = || Err(String::from("rustc compile failed"));

        let cmd_res = Command::new("rustc")
            .arg(file_name)
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

        let cmd_res = Command::new(program).arg(file_name).output();

        if let Ok(cmd_res) = cmd_res {
            if !cmd_res.status.success() {
                if let Some(code) = cmd_res.status.code() {
                    return Err(format!("skitter failed ({})",code));
                } else {
                    return fail();
                }
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

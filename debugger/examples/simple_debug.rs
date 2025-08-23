use piccolo::Value;
use piccolo_debugger::Debugger;

fn main() {
    let source = r#"
            x = 0
            local outer = 10
            function foo(n)
                outer = outer + n
                x = x + n
                return outer + x
            end

            function bar()
                local y = foo(1)
                return y
            end

            local z = bar()
        "#;
    let path = "test.lua";

    let mut dbg = Debugger::new();
    let session_id = dbg.add_session(source, path);

    dbg.launch(session_id);

    dbg.add_breakpoint(session_id, "test.lua".to_string(), 3);
    dbg.add_breakpoint(session_id, "test.lua".to_string(), 9);
    dbg.add_breakpoint(session_id, "test.lua".to_string(), 7);

    dbg.add_function_breakpoint(session_id, "bar".to_string());

    println!("Breakpoints:");
    for (src, lines) in dbg.list_breakpoints(session_id).unwrap() {
        let mut v: Vec<usize> = lines.iter().copied().collect();
        v.sort_unstable();
        println!("  {src}: {:?}", v);
    }

    dbg.remove_breakpoint(session_id, "test.lua".to_string(), 7);

    println!(
        "breakpoints: {:?}",
        dbg.list_breakpoints(session_id).unwrap()
    );

    dbg.watch_global(session_id, "x".to_string(), None);

    println!("Continue…");
    let mut stop = dbg.continue_run(session_id);
    println!("Stopped: {:?}", stop);

    let bt = dbg.backtrace(session_id);
    println!("Backtrace:");
    for (i, loc) in bt.unwrap().iter().enumerate() {
        println!(
            "  #{i} {}:{} {}",
            loc.chunk(),
            loc.line(),
            loc.function_ref()
        );
    }

    if let Some(dis) = dbg
        .disassemble(session_id, "".to_string(), None, None)
        .unwrap()
    {
        println!("Disassembly (current function):");
        for l in dis.iter().take(10) {
            println!("  {l:?}");
        }
    }

    if let Some(readed) = dbg
        .read_memory(session_id, "".to_string(), None, None)
        .unwrap()
    {
        println!("Readed: {:?}", readed);
    }

    if let Some(regs) = dbg.read_registers(session_id).unwrap() {
        println!("Top frame registers ({}):", regs.len());
        for (i, v) in regs.iter().enumerate() {
            println!("  r{i} = {}", v);
        }
    }

    println!("Stack snapshot (before stepping):");
    for (loc, regs) in dbg.stack_snapshot(session_id).unwrap() {
        println!(
            "  {}:{} {} ({} regs)",
            loc.chunk(),
            loc.line(),
            loc.function_ref(),
            regs.len()
        );
    }

    println!("Step into…");
    stop = dbg.step_into(session_id);
    println!("Stopped: {:?}", stop);

    if let Some(ups) = dbg.read_upvalues(session_id).unwrap() {
        println!("Upvalues ({}):", ups.len());
        for (i, v) in ups.iter().enumerate() {
            println!("  uv{i} = {}", v);
        }
    }

    println!("Step over...");
    stop = dbg.step_over(session_id);
    println!("Stopped: {:?}", stop);

    println!("Step out…");
    stop = dbg.step_out(session_id);
    println!("Stopped: {:?}", stop);

    println!("Continue again…");
    stop = dbg.continue_run(session_id);
    println!("Stopped: {:?}", stop);

    if let Some(v0) = dbg.read_register(session_id, 0).unwrap() {
        println!("r0 = {}", v0);
    }
}

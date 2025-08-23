use piccolo_debugger::{Debugger, Error};

fn main() -> Result<(), Error> {
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

    let mut debugger = Debugger::new();
    let session_id = debugger.add_session(source, path);

    debugger.launch(session_id);

    debugger.add_breakpoint(session_id, "test.lua".to_string(), 3)?;
    debugger.add_breakpoint(session_id, "test.lua".to_string(), 9)?;
    debugger.add_breakpoint(session_id, "test.lua".to_string(), 7)?;

    debugger.add_function_breakpoint(session_id, "bar".to_string())?;

    println!("Breakpoints:");
    for (source, lines) in debugger.list_breakpoints(session_id)? {
        let mut v: Vec<usize> = lines.iter().copied().collect();
        v.sort_unstable();
        println!("  {source}: {:?}", v);
    }

    debugger.remove_breakpoint(session_id, "test.lua".to_string(), 7)?;

    println!("breakpoints: {:?}", debugger.list_breakpoints(session_id)?);

    debugger.watch_global(session_id, "x".to_string(), None)?;

    println!("Continue…");
    let mut stop = debugger.continue_run(session_id)?;
    println!("Stopped: {:?}", stop);

    let bt = debugger.backtrace(session_id)?;
    println!("Backtrace:");
    for (index, location) in bt.iter().enumerate() {
        println!(
            "  #{index} {}:{} {}",
            location.chunk(),
            location.line(),
            location.function_ref()
        );
    }

    if let Some(disassembled_instructions) =
        debugger.disassemble(session_id, "".to_string(), None, None)?
    {
        println!("Disassembly (current function):");
        for instruction in disassembled_instructions.iter().take(10) {
            println!("  {instruction:?}");
        }
    }

    if let Some(readed_memory) = debugger.read_memory(session_id, "".to_string(), None, None)? {
        println!("Readed: {:?}", readed_memory);
    }

    if let Some(registers) = debugger.read_registers(session_id)? {
        println!("Top frame registers ({}):", registers.len());
        for (index, value) in registers.iter().enumerate() {
            println!("  r{index} = {}", value);
        }
    }

    println!("Stack snapshot (before stepping):");
    for (location, registers) in debugger.stack_snapshot(session_id)? {
        println!(
            "  {}:{} {} ({} regs)",
            location.chunk(),
            location.line(),
            location.function_ref(),
            registers.len()
        );
    }

    println!("Step into…");
    stop = debugger.step_into(session_id)?;
    println!("Stopped: {:?}", stop);

    if let Some(upvalues) = debugger.read_upvalues(session_id)? {
        println!("Upvalues ({}):", upvalues.len());
        for (index, value) in upvalues.iter().enumerate() {
            println!("  uv{index} = {}", value);
        }
    }

    println!("Step over...");
    stop = debugger.step_over(session_id)?;
    println!("Stopped: {:?}", stop);

    println!("Step out…");
    stop = debugger.step_out(session_id)?;
    println!("Stopped: {:?}", stop);

    println!("Continue again…");
    stop = debugger.continue_run(session_id)?;
    println!("Stopped: {:?}", stop);

    if let Some(v0) = debugger.read_register(session_id, 0)? {
        println!("r0 = {}", v0);
    }

    Ok(())
}

use piccolo::{Closure, Executor, Fuel, Lua, Table, Value};
use piccolo_debugger::Debugger;

fn main() {
    let mut lua = Lua::core();

    lua.enter(|ctx| {
        let source = br#"
            x = 0
            local outer = 10
            local function foo(n)
                outer = outer + n
                x = x + n
                return outer + x
            end

            local function bar()
                local y = foo(1)
                t.k = y
                return y
            end

            local z = bar()
        "#;

        let chunk = Closure::load(ctx, Some("test.lua"), source).unwrap();

        let t = Table::new(&ctx);
        ctx.globals().set_field(ctx, "t", t);

        let mut exec = Executor::start(ctx, chunk.into(), ());

        let mut dbg = Debugger::new(exec);

        dbg.add_breakpoint("test.lua", 3);
        dbg.add_breakpoint("test.lua", 9);
        dbg.add_breakpoint("test.lua", 13);

        let _ = dbg.add_function_breakpoint(ctx, "foo");

        println!("Breakpoints:");
        for (src, lines) in dbg.list_breakpoints() {
            let mut v: Vec<usize> = lines.iter().copied().collect();
            v.sort_unstable();
            println!("  {src}: {:?}", v);
        }

        dbg.remove_breakpoint("test.lua", 9);

        dbg.watch_global("x");
        dbg.watch_table_key(t, Value::String(ctx.intern(b"k")));

        println!("Continue…");
        let mut stop = dbg.continue_run(ctx);
        println!("Stopped: {:?}", stop);

        let bt = dbg.backtrace(ctx);
        println!("Backtrace:");
        for (i, loc) in bt.iter().enumerate() {
            println!(
                "  #{i} {}:{} {}",
                loc.chunk(),
                loc.line(),
                loc.function_ref()
            );
        }

        if let Some(dis) = dbg.disassemble("", None, None) {
            println!("Disassembly (current function):");
            for l in dis.iter().take(10) {
                println!("  {l:?}");
            }
        }

        if let Some(readed) = dbg.read_memory("", None, None) {
            println!("Readed: {:?}", readed);
        }

        if let Some(regs) = dbg.read_registers(ctx) {
            println!("Top frame registers ({}):", regs.len());
            for (i, v) in regs.iter().enumerate() {
                println!("  r{i} = {}", v.display());
            }
        }

        println!("Stack snapshot (before stepping):");
        for (loc, regs) in dbg.stack_snapshot(ctx) {
            println!(
                "  {}:{} {} ({} regs)",
                loc.chunk(),
                loc.line(),
                loc.function_ref(),
                regs.len()
            );
        }

        println!("Step into…");
        stop = dbg.step_into(ctx);
        println!("Stopped: {:?}", stop);

        if let Some(ups) = dbg.read_upvalues(ctx) {
            println!("Upvalues ({}):", ups.len());
            for (i, v) in ups.iter().enumerate() {
                println!("  uv{i} = {}", v.display());
            }
        }

        println!("Step over…");
        stop = dbg.step_over(ctx);
        println!("Stopped: {:?}", stop);

        println!("Step out…");
        stop = dbg.step_out(ctx);
        println!("Stopped: {:?}", stop);

        println!("Continue again…");
        stop = dbg.continue_run(ctx);
        println!("Stopped: {:?}", stop);

        if let Some(v0) = dbg.read_register(ctx, 0) {
            println!("r0 = {}", v0.display());
        }

        exec = dbg.executor();
        let mut fuel = Fuel::with(10_000);
        let _ = exec.step(ctx, &mut fuel);
    });
}

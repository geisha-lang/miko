// Process exit & external command execution (`sys.proc`)

extern "exit" def c_exit(code: Int): Void
extern "system" def c_system(cmd: String): Int

pub def exit(code: Int): Void = c_exit(code)

pub def shell(cmd: String): Int = c_system(cmd)

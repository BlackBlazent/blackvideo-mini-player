/*
 * process_helpers.c  —  BlackVideo Mini Player  v2.4
 *
 * Replaces WinExec + cmd.exe /c with proper CreateProcess-based async launch.
 * Also provides a named-pipe version for reading stderr progress lines.
 *
 * Exported functions (called from Ada via Import Convention => C):
 *   bv_create_process(cmd, hProcess_out)  → int (1=ok, 0=fail)
 *   bv_create_process_with_pipe(cmd, hProcess_out, hPipe_out) → int
 *   bv_process_still_running(hProcess)    → int (1=running, 0=done)
 *   bv_peek_process_output(hPipe, buf, buf_len) → int bytes_read
 *   bv_close_handle(h)
 */

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <string.h>

/* ── bv_create_process ──────────────────────────────────────────────────── */
/*  Launches cmd asynchronously.  Writes the HANDLE to *hProcess_out.       */
/*  Returns 1 on success, 0 on failure.                                     */
int bv_create_process(const char *cmd, HANDLE *hProcess_out)
{
    STARTUPINFOA        si;
    PROCESS_INFORMATION pi;
    char                cmd_buf[8192];
    DWORD               cmd_len;

    if (!cmd || !hProcess_out) return 0;

    /* Guard against cmd.exe 8191-char limit */
    cmd_len = (DWORD)strlen(cmd);
    if (cmd_len >= sizeof(cmd_buf) - 1) {
        /* Write to temp .bat and call that instead */
        char bat_path[MAX_PATH];
        DWORD tmp_len = GetTempPathA(MAX_PATH, bat_path);
        if (tmp_len == 0) return 0;
        strncat(bat_path, "bv_whisper_job.bat", sizeof(bat_path) - tmp_len - 1);

        FILE *bf = fopen(bat_path, "w");
        if (!bf) return 0;
        fprintf(bf, "@echo off\r\n%s\r\n", cmd);
        fclose(bf);

        snprintf(cmd_buf, sizeof(cmd_buf), "cmd.exe /c \"%s\"", bat_path);
    } else {
        strncpy(cmd_buf, cmd, sizeof(cmd_buf) - 1);
        cmd_buf[sizeof(cmd_buf) - 1] = '\0';
    }

    ZeroMemory(&si, sizeof(si));
    si.cb          = sizeof(si);
    si.dwFlags     = STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;

    ZeroMemory(&pi, sizeof(pi));

    if (!CreateProcessA(
            NULL, cmd_buf,
            NULL, NULL,
            FALSE,
            CREATE_NO_WINDOW | DETACHED_PROCESS,
            NULL, NULL,
            &si, &pi))
    {
        return 0;
    }

    /* We only need the process handle for polling; close the thread handle. */
    CloseHandle(pi.hThread);
    *hProcess_out = pi.hProcess;
    return 1;
}

/* ── bv_create_process_with_pipe ────────────────────────────────────────── */
/*  Like bv_create_process but redirects stderr to a readable pipe so we    */
/*  can poll whisper-cli progress lines.                                     */
int bv_create_process_with_pipe(const char  *cmd,
                                 HANDLE      *hProcess_out,
                                 HANDLE      *hPipe_read_out)
{
    HANDLE              pipe_read  = INVALID_HANDLE_VALUE;
    HANDLE              pipe_write = INVALID_HANDLE_VALUE;
    SECURITY_ATTRIBUTES sa;
    STARTUPINFOA        si;
    PROCESS_INFORMATION pi;
    char                cmd_buf[8192];

    if (!cmd || !hProcess_out || !hPipe_read_out) return 0;

    sa.nLength              = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle       = TRUE;   /* child inherits write end */

    if (!CreatePipe(&pipe_read, &pipe_write, &sa, 0)) return 0;

    /* Read end must NOT be inherited */
    SetHandleInformation(pipe_read, HANDLE_FLAG_INHERIT, 0);

    ZeroMemory(&si, sizeof(si));
    si.cb          = sizeof(si);
    si.dwFlags     = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;
    si.hStdInput   = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput  = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError   = pipe_write;   /* whisper-cli progress → our pipe */

    ZeroMemory(&pi, sizeof(pi));

    strncpy(cmd_buf, cmd, sizeof(cmd_buf) - 1);
    cmd_buf[sizeof(cmd_buf) - 1] = '\0';

    if (!CreateProcessA(
            NULL, cmd_buf,
            NULL, NULL,
            TRUE,   /* inherit handles */
            CREATE_NO_WINDOW,
            NULL, NULL,
            &si, &pi))
    {
        CloseHandle(pipe_read);
        CloseHandle(pipe_write);
        return 0;
    }

    CloseHandle(pi.hThread);
    CloseHandle(pipe_write);   /* child has the write end; we no longer need it */

    *hProcess_out   = pi.hProcess;
    *hPipe_read_out = pipe_read;
    return 1;
}

/* ── bv_process_still_running ───────────────────────────────────────────── */
int bv_process_still_running(HANDLE hProcess)
{
    DWORD code = 0;
    if (!hProcess || hProcess == INVALID_HANDLE_VALUE) return 0;
    if (!GetExitCodeProcess(hProcess, &code)) return 0;
    return (code == STILL_ACTIVE) ? 1 : 0;
}

/* ── bv_peek_process_output ─────────────────────────────────────────────── */
/*  Non-blocking read from the pipe.  Returns bytes read (0 = nothing yet). */
int bv_peek_process_output(HANDLE hPipe, char *buf, int buf_len)
{
    DWORD avail = 0;
    DWORD read  = 0;

    if (!hPipe || hPipe == INVALID_HANDLE_VALUE || !buf || buf_len <= 0)
        return 0;

    if (!PeekNamedPipe(hPipe, NULL, 0, NULL, &avail, NULL)) return 0;
    if (avail == 0) return 0;

    DWORD to_read = (avail < (DWORD)(buf_len - 1)) ? avail : (DWORD)(buf_len - 1);
    if (!ReadFile(hPipe, buf, to_read, &read, NULL)) return 0;
    buf[read] = '\0';
    return (int)read;
}

/* ── bv_close_handle ────────────────────────────────────────────────────── */
void bv_close_handle(HANDLE h)
{
    if (h && h != INVALID_HANDLE_VALUE)
        CloseHandle(h);
}

#else
/* ── POSIX stubs ────────────────────────────────────────────────────────── */
#include <stdio.h>
int  bv_create_process(const char *cmd, void **h_out)
     { if(h_out)*h_out=NULL; (void)cmd; return 0; }
int  bv_create_process_with_pipe(const char *c,void **h,void **p)
     { if(h)*h=NULL;if(p)*p=NULL;(void)c; return 0; }
int  bv_process_still_running(void *h) { (void)h; return 0; }
int  bv_peek_process_output(void *h,char *b,int l)
     { (void)h;(void)b;(void)l; return 0; }
void bv_close_handle(void *h) { (void)h; }
#endif


/*
 * llm_client.c  —  BlackVideo Mini Player  v2.4
 *
 * WinINet-based HTTPS client used by:
 *   - LLM_Bridge (POST to Claude / OpenAI / Gemini / DeepSeek / Grok)
 *   - Updater (GET latest.json from GitHub)
 *
 * No libcurl dependency.  WinINet (wininet.dll) ships with all Windows 10/11.
 *
 * Exports:
 *   bv_http_get (url, out_buf, buf_len)  → bytes_read
 *   bv_http_post(url, headers, body, out_buf, buf_len) → bytes_read
 */

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <wininet.h>
#include <stdio.h>
#include <string.h>

#pragma comment(lib, "wininet.lib")

#define BV_USER_AGENT  "BlackVideoPlayer/2.4 WinINet"
#define BV_TIMEOUT_MS  60000

static HINTERNET s_inet = NULL;

static HINTERNET bv_inet_open(void)
{
    if (!s_inet) {
        s_inet = InternetOpenA(BV_USER_AGENT,
                               INTERNET_OPEN_TYPE_PRECONFIG,
                               NULL, NULL, 0);
    }
    return s_inet;
}

/* ── bv_http_get ────────────────────────────────────────────────────────── */
int bv_http_get(const char *url, char *out_buf, int buf_len)
{
    HINTERNET hConn = NULL;
    DWORD     read  = 0;
    int       total = 0;

    if (!url || !out_buf || buf_len <= 0) return 0;

    HINTERNET hInet = bv_inet_open();
    if (!hInet) return 0;

    hConn = InternetOpenUrlA(hInet, url,
                              NULL, 0,
                              INTERNET_FLAG_RELOAD |
                              INTERNET_FLAG_NO_CACHE_WRITE |
                              INTERNET_FLAG_SECURE,
                              0);
    if (!hConn) return 0;

    while (total < buf_len - 1) {
        DWORD chunk = 0;
        if (!InternetReadFile(hConn,
                              out_buf + total,
                              (DWORD)(buf_len - 1 - total),
                              &chunk))
            break;
        if (chunk == 0) break;
        total += (int)chunk;
    }
    out_buf[total] = '\0';

    InternetCloseHandle(hConn);
    return total;
}

/* ── bv_http_post ───────────────────────────────────────────────────────── */
int bv_http_post(const char *url,
                 const char *headers,
                 const char *body,
                 char       *out_buf,
                 int         buf_len)
{
    URL_COMPONENTSA uc;
    char   host[512]    = {0};
    char   path[4096]   = {0};   /* large enough for Gemini ?key=... URLs */
    char   extra[2048]  = {0};   /* query string portion */
    char   full_path[6144] = {0};
    BOOL   use_https;
    HINTERNET hConn     = NULL;
    HINTERNET hReq      = NULL;
    int    total        = 0;
    DWORD  flags;
    DWORD  timeout_ms   = 60000;  /* 60s — DeepSeek needs more than 30s */

    if (!url || !out_buf || buf_len <= 0) return 0;

    /* Parse URL: ICU_ESCAPE preserves %xx and keeps query string in path */
    ZeroMemory(&uc, sizeof(uc));
    uc.dwStructSize       = sizeof(uc);
    uc.lpszHostName       = host;
    uc.dwHostNameLength   = sizeof(host) - 1;
    uc.lpszUrlPath        = path;
    uc.dwUrlPathLength    = sizeof(path) - 1;
    uc.lpszExtraInfo      = extra;  /* catches ?key=... query string */
    uc.dwExtraInfoLength  = sizeof(extra) - 1;
    uc.dwSchemeLength     = 4;

    if (!InternetCrackUrlA(url, 0, ICU_ESCAPE, &uc)) {
        /* fallback: try without flag */
        if (!InternetCrackUrlA(url, 0, 0, &uc)) return 0;
    }

    /* Combine path + query string for HttpOpenRequestA */
    if (extra[0])
        _snprintf(full_path, sizeof(full_path) - 1, "%s%s", path, extra);
    else
        strncpy(full_path, path, sizeof(full_path) - 1);

    use_https = (uc.nScheme == INTERNET_SCHEME_HTTPS);

    HINTERNET hInet = bv_inet_open();
    if (!hInet) return 0;

    hConn = InternetConnectA(hInet, host,
                              use_https ? INTERNET_DEFAULT_HTTPS_PORT
                                        : INTERNET_DEFAULT_HTTP_PORT,
                              NULL, NULL,
                              INTERNET_SERVICE_HTTP, 0, 0);
    if (!hConn) return 0;

    flags = INTERNET_FLAG_RELOAD | INTERNET_FLAG_NO_CACHE_WRITE |
            INTERNET_FLAG_KEEP_CONNECTION;
    if (use_https)
        flags |= INTERNET_FLAG_SECURE;

    hReq = HttpOpenRequestA(hConn, "POST", full_path,
                             "HTTP/1.1", NULL, NULL, flags, 0);
    if (!hReq) { InternetCloseHandle(hConn); return 0; }

    /* Timeouts */
    InternetSetOptionA(hReq, INTERNET_OPTION_CONNECT_TIMEOUT,
                       &timeout_ms, sizeof(DWORD));
    InternetSetOptionA(hReq, INTERNET_OPTION_SEND_TIMEOUT,
                       &timeout_ms, sizeof(DWORD));
    InternetSetOptionA(hReq, INTERNET_OPTION_RECEIVE_TIMEOUT,
                       &timeout_ms, sizeof(DWORD));

    /* Send — headers must end with \r\n for WinINet */
    DWORD body_len = body ? (DWORD)strlen(body) : 0;
    DWORD hdr_len  = headers ? (DWORD)strlen(headers) : 0;

    BOOL ok = HttpSendRequestA(hReq, headers, hdr_len,
                               (LPVOID)body, body_len);

    if (!ok) {
        DWORD err = GetLastError();
        /* For TLS/cert errors, retry without strict cert check */
        if (err == ERROR_INTERNET_INVALID_CA ||
            err == ERROR_INTERNET_SEC_CERT_CN_INVALID ||
            err == ERROR_INTERNET_SEC_CERT_DATE_INVALID) {
            DWORD sec_flags = 0;
            DWORD sf_sz = sizeof(sec_flags);
            InternetQueryOptionA(hReq, INTERNET_OPTION_SECURITY_FLAGS,
                                 &sec_flags, &sf_sz);
            sec_flags |= SECURITY_FLAG_IGNORE_UNKNOWN_CA |
                         SECURITY_FLAG_IGNORE_CERT_CN_INVALID |
                         SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
            InternetSetOptionA(hReq, INTERNET_OPTION_SECURITY_FLAGS,
                               &sec_flags, sizeof(sec_flags));
            ok = HttpSendRequestA(hReq, headers, hdr_len,
                                  (LPVOID)body, body_len);
        }
        if (!ok) {
            InternetCloseHandle(hReq);
            InternetCloseHandle(hConn);
            return 0;
        }
    }

    /* Check HTTP status */
    DWORD status = 0;
    DWORD status_sz = sizeof(status);
    HttpQueryInfoA(hReq, HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
                   &status, &status_sz, NULL);
    /* Read response regardless of status — we want the error body */
    while (total < buf_len - 1) {
        DWORD chunk = 0;
        if (!InternetReadFile(hReq,
                              out_buf + total,
                              (DWORD)(buf_len - 1 - total),
                              &chunk))
            break;
        if (chunk == 0) break;
        total += (int)chunk;
    }
    out_buf[total] = '\0';

    InternetCloseHandle(hReq);
    InternetCloseHandle(hConn);

    /* Return -total for HTTP errors so caller knows it's an API error body */
    /* Actually just return total — caller reads the body either way */
    return total;
}

#else
/* ── POSIX stubs ────────────────────────────────────────────────────────── */
int bv_http_get(const char *u, char *b, int l)
    { (void)u;(void)b;(void)l; return 0; }
int bv_http_post(const char *u,const char *h,const char *b,char *o,int l)
    { (void)u;(void)h;(void)b;(void)o;(void)l; return 0; }
#endif

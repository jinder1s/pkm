from http.server import BaseHTTPRequestHandler, HTTPServer

import subprocess

def run_emacsclient(command):
    try:
        result = subprocess.run(
            ['emacsclient', '-e', command],
            check=True,
            text=True,
            capture_output=True
        )
        print("Output:", result.stdout)
        return result.stdout
    except subprocess.CalledProcessError as e:
        print("Error:", e.stderr)


class RequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length).decode('utf-8')
        command = f"(pkm-sync-server-call `{post_data})"
        return_data = run_emacsclient(command=command)
        # Write the data to a temporary file that is not deleted

        # Respond to the client
        self.send_response(200)
        self.send_header('Content-type', 'text/plain')
        self.end_headers()
        self.wfile.write(return_data.encode())

def run(server_class=HTTPServer, handler_class=RequestHandler, port=8000):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    print(f'Starting server on port {port}')
    httpd.serve_forever()

if __name__ == '__main__':
    run()

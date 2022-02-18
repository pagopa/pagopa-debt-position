import json
import sys

import tornado.ioloop
import tornado.web


class GPDHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self):
        print("request received")
        if self.get_query_argument("since", "01-01-1970") == "01-01-1970":
            organization_list = ["77777777777", "90000000001"]
        else:
            organization_list = []
            for i in range(0, 10):
                organization_list.append(f"9000000000{i}")
        org_map = {
            "add": organization_list,
            "delete": []
        }
        self.write(json.dumps(org_map))


def make_app():
    return tornado.web.Application([
        (r"/organizations", GPDHandler),
    ])


if __name__ == "__main__":
    default_port = '8085' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("gdp running...")
    tornado.ioloop.IOLoop.current().start()

import json
import sys
import random

import tornado.ioloop
import tornado.web


def generate_response():
    organization_list = []
    generator = range(0, 20)
    for i in generator:
        organization_list.append(f"900000000{str(i).zfill(2)}")

    seed = random.choice(generator)
    add_organization_list = []
    for i in range(0, seed):
        organization = random.choice(organization_list)
        add_organization_list.append(organization)
        organization_list.remove(organization)
    delete_organization_list = organization_list

    org_map = {
        "add": add_organization_list,
        "delete": delete_organization_list
    }
    return org_map


class GPDHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self):
        print("request received")
        self.write(json.dumps(generate_response()))


def make_app():
    return tornado.web.Application([
        (r"/organizations", GPDHandler),
    ])


if __name__ == "__main__":
    default_port = '8085' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("running...")
    tornado.ioloop.IOLoop.current().start()

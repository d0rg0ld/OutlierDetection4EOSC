#!/usr/bin/env python3
import connexion
import signal

from swagger_server import encoder

CERT = 'certs/cert.pem'
KEY = 'certs/key.pem'

def main():
    #https://stackoverflow.com/questions/16807603/python-non-blocking-non-defunct-process
	
    signal.signal(signal.SIGCHLD, signal.SIG_IGN)

    app = connexion.App(__name__, specification_dir='./swagger/')
    app.app.json_encoder = encoder.JSONEncoder
    app.add_api('swagger.yaml', arguments={'title': 'Timeseries Outlier Analysis'}, pythonic_params=True)
    app.run(port=8080, debug=True, ssl_context=(CERT, KEY))


if __name__ == '__main__':
    main()

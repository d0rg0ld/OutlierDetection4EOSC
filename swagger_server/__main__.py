#!/usr/bin/env python3
import connexion
import signal
import json
import os

from swagger_server import encoder

from . import globVars

CERT = 'certs/cert.pem'
KEY = 'certs/key.pem'

def main():
    #https://stackoverflow.com/questions/16807603/python-non-blocking-non-defunct-process
	
    signal.signal(signal.SIGCHLD, signal.SIG_IGN)

    app = connexion.App(__name__, specification_dir='./swagger/')
    app.app.json_encoder = encoder.JSONEncoder
    app.add_api('swagger.yaml', arguments={'title': 'Timeseries Outlier Analysis'}, pythonic_params=True)

    # read in some config files and write this into glob vars
    myCwd=os.getcwd()
    globVars.resultdir=myCwd+"/results/"
    globVars.cachedir=myCwd+"/cache/"
    globVars.scriptdir=myCwd+"/outlier_script/"

    configData=json.load(open("serviceConfig.json", "r"))

    if "ServiceConfig" in configData:
        if "results" in configData["ServiceConfig"]:
            globVars.resultdir=configData["ServiceConfig"]["results"]
            if myCwd not in globVars.resultdir:
                globVars.resultdir=myCwd+"/"+globVars.resultdir
        if "cache" in configData["ServiceConfig"]:
            globVars.cachedir=configData["ServiceConfig"]["cache"]
            if myCwd not in globVars.cachedir:
                globVars.cachedir=myCwd+"/"+globVars.cachedir
        if "script" in configData["ServiceConfig"]:
            globVars.scriptdir=configData["ServiceConfig"]["script"]
            if myCwd not in globVars.scriptdir:
                globVars.scriptdir=myCwd+"/"+globVars.scriptdir

    app.run(port=8080, debug=True, ssl_context=(CERT, KEY))


if __name__ == '__main__':
    main()

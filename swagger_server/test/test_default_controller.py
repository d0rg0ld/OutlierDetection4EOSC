# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

from swagger_server.test import BaseTestCase


class TestDefaultController(BaseTestCase):
    """DefaultController integration test stubs"""

    def test_qs_get(self):
        """Test case for qs_get

        Retrieve a parameter from one station from start to end
        """
        query_string = [('sosendpoint', 'sosendpoint_example'),
                        ('begin', '2013-10-20T19:20:30+01:00'),
                        ('end', '2013-10-20T19:20:30+01:00'),
                        ('parameter', 'parameter_example'),
                        ('site', 'site_example'),
                        ('windowwidth', 2),
                        ('windowinterval', 2)]
        response = self.client.open(
            '/v1/qs',
            method='GET',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_qs_post(self):
        """Test case for qs_post

        Perform outlier analysis on file(s) stored in remote repo
        """
        query_string = [('repourl', 'repourl_example'),
                        ('windowwidth', 2),
                        ('windowinterval', 2)]
        response = self.client.open(
            '/v1/qs',
            method='POST',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()

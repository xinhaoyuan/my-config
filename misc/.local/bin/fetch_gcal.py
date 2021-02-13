#!/usr/bin/env python3
#
# A simple script modified from
# https://developers.google.com/calendar/quickstart/python to fetch
# incoming calendar event into a org-mode file.
#
# You would need to create a Google Cloud Platform project and
# download its credentials to $HOME/.gcal_credential.json.
#
# To install dependencies, use
#   pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib
#
# Usage example: fetch_gcal.py -o org/gcal.org

import datetime
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
import argparse
import os

parser = argparse.ArgumentParser()
parser.add_argument("-o", dest="output", required=True)
parser.add_argument("-n", dest="num", default=1)
parser.add_argument("-c", dest="credentials", default=os.environ.get("HOME")+"/.gcal_credentials.json")

TOKEN_FILE=os.environ.get("HOME")+"/.cache/gcal_token.pickle"
# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/calendar.readonly']

def main(args):
    """Shows basic usage of the Google Calendar API.
    Prints the start and name of the next 10 events on the user's calendar.
    """
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                args.credentials, SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    with open(args.output, "w") as output:
        output.write("# Event fetched from Google calendar.\n\n")

        service = build('calendar', 'v3', credentials=creds)

        # Call the Calendar API
        now = datetime.datetime.utcnow().isoformat() + 'Z' # 'Z' indicates UTC time
        print('Getting the upcoming {} events'.format(args.num))
        events_result = service.events().list(calendarId='primary', timeMin=now,
                                              maxResults=args.num, singleEvents=True,
                                              orderBy='startTime').execute()
        events = events_result.get('items', [])

        if not events:
            print('No upcoming events found.')
        for event in events:
            start = event['start'].get('dateTime')
            has_time = start is not None
            if start is None:
                start = event['start'].get('date')
            start = datetime.datetime.fromisoformat(start)
            output.write("* TODO Gcal: {}\n  SCHEDULED: <{}>\n".format(
                event['summary'],
                start.strftime("%Y-%m-%d %a %H:%M" if has_time else "%Y-%m-%d %a")))

if __name__ == '__main__':
    args = parser.parse_args()
    main(args)

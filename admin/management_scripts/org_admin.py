import sys
import requests
import json
import csv
import argparse

parser = argparse.ArgumentParser(description='Add a university, company or school')
parser.add_argument('name', metavar='N', type=str,help='Name of the university, company or org')
parser.add_argument('type', metavar='T', type=str,help='Type of org')
parser.add_argument('domain', metavar='D', type=str,help='Domain of the org')
parser.add_argument('api-key', metavar='K', type=str,help='API Key')

api_server_url_base = "https://api.halite.io/v1/api/organization"
email_endpoint = "/email_domain"

def main(argv):
    add_org(args.name, args.type, args.domain, args.key)
    
def bulk_add_halite1_data(key):
    with open('halite1orgdata.csv', 'rU') as f:
        reader = csv.reader(f)
        extlist = list(reader)
        count = 0
        for row in extlist:
            if(count>0):
                add_org(row[0], row[2], row[1], key)
            count = count + 1

def bulk_add_publicuni_data(key):
    count = 0
    response = requests.request("GET", "https://raw.githubusercontent.com/Hipo/university-domains-list/master/world_universities_and_domains.json")
    response_data = json.loads(response.text)
    for org in response_data:
        try:
            add_org(org["name"], "University",org["domain"],key)
        except ValueError:
            count = count + 1
            print("Oops! " + str(count))

    print("Total Failures: " + count)

# Add email to org
def add_org(name, type, domain, key):
    data = {}
    data['name'] = name
    data['type'] = type
    data['require_code'] = "false"
    org_data = json.dumps(data)
    print (org_data)
    response = requests.request("POST", api_server_url_base, data=org_data, headers=get_header(key))
    print(response.text)
    response_data = json.loads(response.text)
    print(response_data)
    add_org_email(response_data["organization_id"], domain, key)

def add_org_email(id, domain,key):
    data = {}
    data['domain'] = domain
    org_data = json.dumps(data)
    print(org_data)
    response = requests.request("POST", api_server_url_base +"/"+ str(id) + email_endpoint, data=org_data, headers=get_header(key))
    print(response.text)

# Get all orgs and email domain
def get_org_info(key):
    response = requests.request("GET", api_server_url_base, headers=get_header(key))
    org_data = json.loads(response.text)
    for org in org_data:
        response = requests.request("GET", api_server_url_base + "/" + str(org["organization_id"]) + email_endpoint, headers=get_header(key))
        email_data = json.loads(response.text)
        org["emails"] = []
        for email in email_data:
            org["emails"].append(email)
    print(org_data)
    return org_data

def get_header(key):
    post_headers = {
        'x-api-key': key,
        'cache-control': "no-cache",
        'content-type': "application/json",
        }
    return post_headers

if __name__ == "__main__":
    args = parser.parse_args()
    main(args)
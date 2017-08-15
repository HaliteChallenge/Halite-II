import sys
import requests
import json

get_headers = {
'x-api-key': "1005:2ded42eb5f0a4dfd87e8a42643e495a6",
'cache-control': "no-cache"
}

post_headers = {
'x-api-key': "1005:2ded42eb5f0a4dfd87e8a42643e495a6",
'cache-control': "no-cache",
'content-type': "application/json",
}

api_server_url_base = "http://35.190.3.178/v1/api/organization"
email_endpoint = "/email_domain"

def main(argv):
    # get_org_info()
    # bulk_add_publicuni_data()
    

def bulk_add_publicuni_data():
    count = 0
    response = requests.request("GET", "https://raw.githubusercontent.com/Hipo/university-domains-list/master/world_universities_and_domains.json")
    response_data = json.loads(response.text)
    for org in response_data:
        try:
            add_org(org["name"], "University",org["domain"])
            count = count + 1
        except ValueError:
            print("Oops! " + str(count))

    print("Total Failures: " + count)

# Add email to org
def add_org(name, type, domain):
    data = {}
    data['name'] = name
    data['type'] = type
    data['require_code'] = "false"
    org_data = json.dumps(data)
    response = requests.request("POST", api_server_url_base, data=org_data, headers=post_headers)
    response_data = json.loads(response.text)
    print(response_data)
    add_org_email(response_data["organization_id"], domain)

def add_org_email(id, domain):
    data = {}
    data['domain'] = domain
    org_data = json.dumps(data)
    print(org_data)
    response = requests.request("POST", api_server_url_base +"/"+ str(id) + email_endpoint, data=org_data, headers=post_headers)
    print(response.text)


# Get all orgs and email domain
def get_org_info():
    response = requests.request("GET", api_server_url_base, headers=get_headers)
    org_data = json.loads(response.text)
    for org in org_data:
        response = requests.request("GET", api_server_url_base + "/" + str(org["organization_id"]) + email_endpoint, headers=post_headers)
        print(response.text)
        email_data = json.loads(response.text)
        org["emails"] = []
        for email in email_data:
            org["emails"].append(email)
    print(org_data)
    return org_data


if __name__ == "__main__":
    main(sys.argv)
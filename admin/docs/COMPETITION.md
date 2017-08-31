# Running the Competition

## Ending the Competition

Update `apiserver/apiserver/config.py`. Set `COMPETITION_OPEN = False`. Re-upload the blob and recreate the coordinator servers.

## Creating Hackathons

Use `admin/create_hackathon.py`.

    $ python3 create_hackathon.py auth
    ...Paste your API key from the website...
    $ python3 create_hackathon.py list
    $ python3 create_hackathon.py create

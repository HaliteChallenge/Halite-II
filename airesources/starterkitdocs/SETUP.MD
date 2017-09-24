# Doc Setup

Assumes recent Ubuntu. Adapt as needed for other OSes.

Get python3 and pip if needed:
1. apt install python3
2. apt install python-pip
3. export PATH="${HOME}/.local/bin:${PATH}"

Create python3 virtual environment:
1. virtualenv -p python3 <hlt-py3-env>
2. source <hlt-py3-env>/bin/activate
3. pip install -r requirements.txt

Get Mono:
- Via apt:
    1. apt install mono-complete
- Via brew - takes long time:
    1. apt install linuxbrew-wrapper
    2. export PATH="${HOME}/.linuxbrew/bin:${PATH}"
    3. brew install mono
- Any other way is fine

Generate the docs:
1. javasphinx-apidoc -f -o source/ --title='Java Starter Kit' ../Java/hlt

import sendgrid

from . import config


sg = sendgrid.SendGridAPIClient(apikey=config.SENDGRID_API_KEY)


def send_notification(recipient_email, recipient_name, subject, body,
                      attachments=None):
    mail = sendgrid.helpers.mail.Mail()

    mail.from_email = sendgrid.Email("halite@halite.io", "Halite Challenge")
    personalization = sendgrid.helpers.mail.Personalization()
    personalization.add_to(sendgrid.helpers.mail.Email(recipient_email, recipient_name))
    personalization.subject = "Halite Challenge: " + subject
    mail.add_personalization(personalization)

    mail.add_content(sendgrid.helpers.mail.Content("text/html", body))

    settings = sendgrid.helpers.mail.MailSettings()
    settings.sandbox_mode = sendgrid.helpers.mail.SandBoxMode(config.SENDGRID_SANDBOX_MODE)
    mail.mail_settings = settings

    response = sg.client.mail.send.post(request_body=mail.get())
    print(response.status_code)
    print(response.headers)
    print(response.body)


COMPILATION_SUCCESS = """
Your bot successfully compiled!

- Halite II Alpha
"""


COMPILATION_FAILURE = """
Your bot failed to compile!

Detected language: {language}

Error logs:

{errors}

- Halite II Alpha
"""


VERIFY_EMAIL = """
Please verify your email!

{verification_code}

- Halite II Alpha
"""
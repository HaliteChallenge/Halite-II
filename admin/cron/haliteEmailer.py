import smtplib
from email.mime.text import MIMEText

def sendEmail(senderEmail, senderPassword, subject, body, recipient):
    print("Sending email")

    msg = MIMEText(body, "html")
    msg['Subject'] = subject
    msg['From'] = senderEmail
    msg['To'] = recipient

    s = smtplib.SMTP('smtp.gmail.com:587')
    s.ehlo()
    s.starttls();
    s.login(senderEmail, senderPassword)
    s.sendmail(senderEmail, [recipient], msg.as_string())
    s.quit()

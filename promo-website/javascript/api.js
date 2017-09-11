export const API_SERVER_URL = "http://35.190.3.178/v1/api";

export function makeRequest() {
    const xhr = new XMLHttpRequest();
    return xhr;
}

export function addsubscriber(recipient) {
    const xhr = makeRequest();
    xhr.open("POST", `${API_SERVER_URL}/user/addsubscriber/${recipient}`);
    xhr.send();
}
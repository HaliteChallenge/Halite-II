const API_SERVER_URL = "http://35.185.123.54:5000/api/v1";
const LOGIN_SERVER_URL = "http://35.185.123.54:5000/login";

export function me() {
    return $.get(`${LOGIN_SERVER_URL}/me`).then((me) => {
        if (me === null) {
            return null;
        }
        return get_user(me.user_id);
    });
}

export function get_user(user_id) {
    return $.get(`${API_SERVER_URL}/user/${user_id}`);
}
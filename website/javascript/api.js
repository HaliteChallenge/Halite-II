const API_SERVER_URL = "http://35.185.123.54:5000/api/v1";
const LOGIN_SERVER_URL = "http://35.185.123.54:5000/login";

// TODO: cache login in local cookie so we don't have to do so many round trips
export function me() {
    return $.get({
        url: `${LOGIN_SERVER_URL}/me`,
        xhrFields: {
            withCredentials: true,
        },
    }).then((me) => {
        if (me === null) {
            return null;
        }
        return get_user(me.user_id);
    });
}

export function get_user(user_id) {
    return $.get({
        url: `${API_SERVER_URL}/user/${user_id}`,
        xhrFields: {
            withCredentials: true,
        },
    });
}
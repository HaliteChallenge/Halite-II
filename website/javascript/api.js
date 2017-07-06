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

export function list_organizations() {
    return $.get({
        url: `${API_SERVER_URL}/organization`,
    });
}

export function register_me(data) {
    return $.post({
        url: `${API_SERVER_URL}/user`,
        data: JSON.stringify(data),
        contentType: "application/json",
        xhrFields: {
            withCredentials: true,
        },
    });
}

export function get_replay(game_id) {
    return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        xhr.withCredentials = true;
        xhr.open("GET", `${API_SERVER_URL}/user/0/match/${game_id}/replay`, true);
        xhr.responseType = "arraybuffer";

        xhr.onload = function(e) {
            if (this.status == 200) {
                const blob = this.response;
                resolve(blob);
            }
            else {
                reject();
            }
        };

        xhr.send();
    });
}

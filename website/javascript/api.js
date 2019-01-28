/* jshint esversion: 6 */

export const API_SERVER_URL = api_server_url
export const LOGIN_SERVER_URL = login_server_url
export const LOGOUT_SERVER_URL = logout_server_url

let cached_me = null
let logged_in = null

export function me_cached () {
  if (cached_me) {
    return {
      user_id: cached_me['user_id'],
      username: cached_me['username']
    }
  } else if (window.localStorage['cache']) {
    return {
      user_id: window.localStorage['user_id'],
      username: window.localStorage['username']
    }
  } else {
    return null
  }
}

export function me () {
  return Promise.resolve(null)
}

export function get_user (user_id) {
  return $.get({
    url: `${API_SERVER_URL}/user/${user_id}`,
    xhrFields: {
      withCredentials: true
    }
  })
}

export function logout () {
  return $.post({
    url: `${LOGOUT_SERVER_URL}`,
    xhrFields: {
      withCredentials: true
    }
  }).then((res) => {
    logged_in = false
    window.localStorage.removeItem('cache')
    window.localStorage.removeItem('user_id')
    window.localStorage.removeItem('username')
  })
}

export function make_profile_image_url (username) {
  return `https://github.com/${username}.png`
}

export function list_bots (user_id) {
  return $.get({
    url: `${API_SERVER_URL}/user/${user_id}/bot`
  })
}

export function makeRequest () {
  const xhr = new XMLHttpRequest()
  return xhr
}

export function update_bot (user_id, bot_id, file, progress_callback) {
  const method = bot_id === null ? 'POST' : 'PUT'
  const endpoint = bot_id === null ? 'bot' : `bot/${bot_id}`

  const xhr = makeRequest()
  xhr.upload.addEventListener('progress', function (e) {
    if (e.lengthComputable) {
      progress_callback(e.loaded / e.total)
    }
  }, false)
  xhr.upload.addEventListener('load', function (e) {
    progress_callback(1)
  }, false)
  xhr.withCredentials = true
  xhr.open(method, `${API_SERVER_URL}/user/${user_id}/${endpoint}`)

  const form_data = new FormData()
  form_data.append('name', 'botFile')
  form_data.append('botFile', file)

  xhr.send(form_data)

  return new Promise((resolve, reject) => {
    xhr.onload = function (e) {
      if (this.status === 200 || this.status == 201) {
        resolve()
      } else {
        const response = JSON.parse(e.target.responseText)
        reject(response)
      }
    }
  })
}

export function list_organizations (user_id) {
  return $.get({
    url: `${API_SERVER_URL}/organization`
  })
}

export function get_season1_stats (userId) {
  return $.get({
    url: `${API_SERVER_URL}/user/${userId}/season1`,
    xhrFields: {
      withCredentials: true
    }
  })
}


export function register_me (data) {
  return $.post({
    url: `${API_SERVER_URL}/user`,
    data: JSON.stringify(data),
    contentType: 'application/json',
    xhrFields: {
      withCredentials: true
    }
  })
}

export function update_me (user_id, data) {
  return $.ajax({
    url: `${API_SERVER_URL}/user/${user_id}`,
    method: 'PUT',
    data: JSON.stringify(data),
    contentType: 'application/json',
    xhrFields: {
      withCredentials: true
    }
  })
}

export function resend_verification_email(user_id) {
    return $.ajax({
        url: `${API_SERVER_URL}/user/${user_id}/verify/resend`,
        method: "POST",
        xhrFields: {
            withCredentials: true,
        },
    });
}

export function get_replay (game_id, progress_callback) {
  let game_data_promise = Promise.resolve($.get(`${API_SERVER_URL}/user/0/match/${game_id}`))
  let replay_promise = new Promise((resolve, reject) => {
    const xhr = makeRequest()
    xhr.withCredentials = true
    xhr.open('GET', `${API_SERVER_URL}/user/0/match/${game_id}/replay`, true)
    xhr.responseType = 'arraybuffer'

    if (progress_callback) {
      xhr.onprogress = function (e) {
        progress_callback(e.loaded, e.total)
      }
    }

    xhr.onload = function (e) {
      if (this.status === 200) {
        const blob = this.response
        resolve(blob)
      } else {
        reject()
      }
    }

    xhr.onerror = function () {
      reject()
    }

    xhr.onreadystatechange = function (e) {
      if (xhr.readyState === 4) {
        if (xhr.status !== 200) {
          reject()
        }
      }
    }

    xhr.send()
  })
  return Promise.all([game_data_promise, replay_promise]).then(([game, replay]) => {
    return {
      game: game,
      replay: replay
    }
  })
}

export function get_expired_replay (replay_class, replay_name) {
  return new Promise((resolve, reject) => {
    const xhr = makeRequest()
    xhr.withCredentials = true
    xhr.open('GET', `${API_SERVER_URL}/replay/class/${replay_class}/name/${replay_name}`, true)
    xhr.responseType = 'arraybuffer'

    xhr.onload = function (e) {
      if (this.status === 200) {
        const blob = this.response
        resolve(blob)
      } else {
        reject()
      }
    }

    xhr.onerror = function () {
      reject()
    }

    xhr.onreadystatechange = function (e) {
      if (xhr.readyState === 4) {
        if (xhr.status !== 200) {
          reject()
        }
      }
    }

    xhr.send()
  })
}

export function leaderboard (filters, hackathon = null, offset = null, limit = null) {
  console.log(filters);
  return $.get({
    url: '/static_data/leaderboard.json',
  }).then((data) => {
    if (filters && filters.length > 0) {
      const conditions = [];
      for (const filter of filters) {
        const [ field, _, value ] = filter.split(/,/g);
        if (field === "organization_id") {
          value = parseInt(value, 10);
        }
        if (field === "country_code") {
          field = "country";
        }
        conditions.push([ field, value ]);
      }
      data = data.filter(player => {
        for (const [ field, value ] of conditions) {
          if (player[field] !== value) {
            return false;
          }
        }
        return true;
      });
    }

    if (offset) {
      data = data.slice(offset);
    }
    if (limit) {
      data = data.slice(0, limit);
    }
    return data;
  })
}

export function reset_api_key () {
  return $.post({
    url: `${API_SERVER_URL}/api_key`,
    xhrFields: {
      withCredentials: true
    }
  })
}

export function registerHackathon (code) {
  const me = me_cached()
  if (!me) {
    return Promise.reject({
      message: 'You must be logged in to register for a hackathon.'
    })
  }
  return $.post({
    url: `${API_SERVER_URL}/user/${me.user_id}/hackathon`,
    data: {
      verification_code: code
    },
    xhrFields: {
      withCredentials: true
    }
  })
}

export function getHackathon (id) {
  return $.get({
    url: `${API_SERVER_URL}/hackathon/${id}`,
    xhrFields: {
      withCredentials: true
    }
  })
}

export function getUserHackathons (userId) {
  return $.get({
    url: `${API_SERVER_URL}/user/${userId}/hackathon`,
    xhrFields: {
      withCredentials: true
    }
  })
}

export function getHackathons () {
  return $.get({
    url: `${API_SERVER_URL}/hackathon`,
    xhrFields: {
    }
  })
}

export function getUserHistory (userId) {
  return $.get({
    url: `${API_SERVER_URL}/user/${userId}/history`,
    xhrFields: {
      withCredentials: true
    }
  })
}

export function invitefriend (email) {
  return $.post({
    url: `${API_SERVER_URL}/invitation/user/` + email
  })
}

export function subscribe (email) {
  return $.post({
    url: `${API_SERVER_URL}/user/addsubscriber/` + email
  })
}

export function challenge(user_id, opponents) {
  return $.post({
    url: `${API_SERVER_URL}/user/` + user_id + `/challenge`,
    method: 'POST',
    data: JSON.stringify({opponents: opponents}),
    contentType: 'application/json',
    xhrFields: {
      withCredentials: true
    }
  })
}

/** leagues **/
export function getLeaguesList(){
  return $.get({
    url: `${API_SERVER_URL}/leagues` ,
    xhrFields: {
    }
  });
}

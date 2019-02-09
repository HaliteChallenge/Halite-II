/* jshint esversion: 6 */

export const API_SERVER_URL = api_server_url;
export const LOGIN_SERVER_URL = login_server_url;
export const LOGOUT_SERVER_URL = logout_server_url;

export function me_cached () {
  return null;
}

export function me () {
  return Promise.resolve(null);
}

let users = null;
let unsafe_users = {};
let games = null;

export function get_user (user_id) {
  if (users === null) {
    users = new Promise((resolve) => {
      window.fetch('/static_data/users3.json').then(r => r.json()).then(users => {
        resolve(users);
        unsafe_users = users;
      });
    });
  }
  return users.then(users => {
    return users[user_id];
  });
}

export function unsafe_get_user(user_id) {
  return unsafe_users[user_id];
}

export function make_profile_image_url (username) {
  return `https://github.com/${username}.png`;
}

export function list_bots (user_id) {
  return get_user(user_id).then((user) => user.bots);
}

export function makeRequest() {
  return new XMLHttpRequest();
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

export function get_games(game_ids) {
  if (games === null) {
    games = new Promise((resolve) => {
      window.fetch('/static_data/games.json').then(r => r.json()).then(resolve);
    });
  }
  return games.then((games) => {
    return game_ids.map(game_id => games[game_id]).filter(x => x);
  });
}

export function get_replay (game_id, progress_callback) {
  return get_games([game_id]).then(([game]) => {
    return window.fetch(`${game.replay_class === 1 ? "https://storage.googleapis.com/halite-2-gold-replays" : "https://storage.googleapis.com/halite-2-replays"}/${game.replay}`).then(r => r.arrayBuffer()).then(replay => ({
      game,
      replay,
    }));
  });
}

export function get_expired_replay (replay_class, replay_name) {
  return window.fetch(`${replay_class === '1' ? "https://storage.googleapis.com/halite-2-gold-replays" : "https://storage.googleapis.com/halite-2-replays"}/${replay_name}`).then(r => r.arrayBuffer());
}

export function leaderboard (filters, hackathon = null, offset = null, limit = null) {
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
  });
}


export function getUserHistory (userId) {
  return get_user(userId).then((user) => user.bot_history);
}

/** leagues **/
export function getLeaguesList(){
  return $.get({
    url: `${API_SERVER_URL}/leagues` ,
    xhrFields: {
    }
  });
}

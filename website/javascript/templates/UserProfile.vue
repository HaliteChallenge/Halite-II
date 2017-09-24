<template>
    <div class="row">
        <div class="col-md-3">
            <div class="user-profile">
                <div class="user-profile-avatar">
                    <i class="xline xline-top"></i>
                    <img class="img-responsive" :src="'https://github.com/' + user.username + '.png'" :alt="user.username" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'">
                </div>
                <div class="user-profile-detail">
                    <i class="xline xline-top"></i>
                    <h1>{{ user.username }}</h1>
                    <p>@{{ user.username }}</p>
                </div>
                <div class="user-profile-rank">
                    <i class="xline xline-top"></i>
                    <h2><span class="icon-tier-5"></span> rank 5, diamond tier</h2>
                    <div class="user-profile-rank-stats">
                        <div class="stats-item">
                            <h3>Points</h3>
                            <p>{{ Math.round(user.score * 100) / 100 }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Bots</h3>
                            <p>{{ user.num_submissions }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Games</h3>
                            <p>{{ user.num_games }}</p>
                        </div>
                    </div>
                    <p><a href="#">View on leaderboard</a></p>
                </div>
                <div class="user-profile-about">
                    <i class="xline xline-top"></i>
                    <h2>About</h2>
                    <p>{{ user.level }} <span v-if="user.organization">at {{ user.organization }}</span></p>
                    <p>From New York, USA</p>
                    <p>Bots in 
                        <template v-for="(lang, index) in botLang">
                            <span class="hl">{{lang}}</span><span v-if="(index+1) < botLang.length">,</span>
                        </template>
                    </p>
                </div>
            </div>

            
        </div>
        <div class="col-md-6">
            <section>
                <!-- <h2>Stats Summary</h2>
                <p v-if="user.tier"><b>{{ user.tier }}</b> player</p>
                <p><b>Points:</b> {{ Math.round(user.score * 100) / 100 }}</p>
                <p v-if="user.rank"><b>Rank:</b> {{ user.rank }}</p>
                <p><b>Games Played:</b>  {{ user.num_games }}</p>
                <p><b>Submissions:</b>  {{ user.num_submissions }}</p>
                <p v-for="bot in bots">
                    Bot version {{ bot.version_number }} rank {{ bot.rank }} written in {{ bot.language }} with {{ bot.games_played }} games played
                    <span v-if="bot.compilation_status">(latest version compilation status: {{ bot.compilation_status }})</span>
                </p> -->
            </section>

            <section class="profile-section">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Game Videos Feed
                </h2>

                <!-- <template v-if="is_my_page">
                    <input type="checkbox" v-bind:checked="only_timed_out" id="only-timed-out" v-on:change="toggle_filter" />
                    <label for="only-timed-out">Only show games where I timed out</label>
                </template> -->

                <table class="table table-leader">
                    <thead>
                        <tr>
                            <th>Played On</th>
                            <th>Opponents</th>
                            <th>Map Size</th>
                            <th>Watch</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr v-for="game in games">
                            <td>
                                <time :datetime="game.time_played"
                                      :title="game.time_played">
                                    {{ game.time_played | moment("calendar") }}
                                </time>
                            </td>
                            <td>
                                <a v-for="player in Object.keys(game.players)"
                                   :href="'/user?user_id=' + player"
                                   class="game-participant"
                                   v-bind:class="{ 'timed-out': game.players[player].timed_out }"
                                   :title="game.players[player].timed_out ? 'This player timed out or errored in this game. See the log for details.' : ''">
                                    <img :alt="player" :src="profile_images[player]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" />
                                    <span class="rank">
                                        {{ game.players[player].rank }}
                                    </span>
                                </a>
                            </td>
                            <td>{{ game.map_width }}x{{ game.map_height }}</td>
                            <td>
                                <a :href="'/play?game_id=' + game.game_id">
                                    {{ game.game_id }}
                                </a>

                                <a v-if="game.players[user.user_id].timed_out && is_my_page"
                                   target="_blank"
                                   :href="error_log_link(game.game_id)"
                                   class="text-danger">
                                    Download error log
                                </a>
                            </td>
                        </tr>
                    </tbody>
                </table>

                <div class="btn-group text-center" role="group" aria-label="Game Navigation">
                    <button
                        type="button"
                        class="btn"
                        :disabled="page === 0"
                        v-on:click="prev_page"><span>Prev</span></button>
                    <button
                        type="button"
                        class="btn"
                        v-on:click="next_page"><span>Next</span></button>
                </div>
            </section>

            <section class="profile-section profile-section-hackathon">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Your Hackathon
                </h2>

                <form v-if="is_my_page" class="profile-section-right-form">
                    <div class="form-inline-button">
                        <input type="text" placeholder="Hackathon signup code" ref="hackathon_signup_code">
                        <button class="btn" v-on:click="join_hackathon"><span>Join Hackathon</span></button>
                    </div>
                    <div>
                        <p>{{ messages.hackathon }}</p>
                    </div>
                </form>
                <div v-if="hackathons.length > 0">
                    <table class="table table-leader">
                        <thead>
                            <tr>
                                <th>Hackathons</th>
                                <th>Location</th>
                                <th>Status</th>
                                <th></th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr v-for="hackathon in hackathons">
                                <td>{{hackathon.title}}</td>
                                <td>{{hackathon.location}}</td>
                                <td>{{hackathon.status}}</td>
                                <td><a href="#">Leaderboard</a></td>
                            </tr>
                        </tbody>
                    </table>
                </div>
                    
            </section>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "UserProfile",
        data: function() {
            return {
                user: {
                    "level": "",
                    "username": "",
                    "organization": "",
                    "points": "",
                    "num_games": "",
                    "user_id": "",
                },
                games: [],
                bots: [],
                hackathons: [],
                profile_images: {},
                page: 0,
                limit: 10,
                offset: 0,
                only_timed_out: false,
                is_my_page: false,
                messages: {
                    hackathon: "",
                },
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);
            let source;

            if (params.has("me")) {
                source = api.me();
            }
            else {
                const user_id = params.get("user_id");
                source = api.get_user(user_id);
            }

            source.then((user) => {
                if (user === null) {
                    window.location.replace(`${api.LOGIN_SERVER_URL}/github`);
                    return;
                }
                this.user = user;

                if (params.has("me")) {
                    window.history.replaceState(
                        {}, "",
                        `${window.location.origin}${window.location.pathname}?user_id=${user.user_id}`);
                }
                api.list_bots(user.user_id).then((bots) => {
                    this.bots = bots;
                });
                this.fetch();
                this.fetchHackathon();
            });

            api.me().then((me) => {
                this.is_my_page = me && me.user_id === this.user.user_id;
            });
        },
        computed: {
            botLang: function(){
                let lang = [];
                if (this.bots.length > 0){
                    for (let i = 0; i < this.bots.length; i++){
                        if (lang.indexOf(this.bots[i].language) == -1){
                            lang.push(this.bots[i].language);
                        }
                    }
                }
                return lang;
            }
        },
        methods: {
            fetch: function() {
                let query = `order_by=desc,time_played&offset=${this.offset}&limit=${this.limit}`;
                if (this.only_timed_out) {
                    query += `&filter=timed_out,=,${this.user.user_id}`;
                }
                const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`;
                return $.get(url)
                    .then((data) => {
                        this.games = data;
                        for (let game of data) {
                            for (let participant of Object.keys(game.players)) {
                                if (this.profile_images[participant]) continue;
                                this.profile_images[participant] = "loading";

                                api.get_user(participant).then((user) => {
                                this.profile_images[participant] = api.make_profile_image_url(user.username);
                                this.$forceUpdate();
                            });
                        }
                    }
                });
            },

            fetchHackathon: function(){
                api.getUserHackathons(this.user.user_id).then(hackathons => {
                    if(hackathons && hackathons instanceof Array) {
                        this.hackathons = hackathons;
                    }
                });
            },

            next_page: function() {
                this.offset += 10;
                this.fetch().then(() => {
                    this.page += 1;
                });
            },

            prev_page: function() {
                this.offset -= 10;
                this.fetch().then(() => {
                    this.page -= 1;
                });
            },

            toggle_filter: function() {
                this.only_timed_out = !this.only_timed_out;
                this.offset = 0;
                this.fetch().then(() => {
                    this.page = 0;
                });
            },

            error_log_link: function(game_id) {
                return `${api.API_SERVER_URL}/user/${this.user.user_id}/match/${game_id}/error_log`;
            },

            join_hackathon: function(event) {
                event.preventDefault();

                const code = this.$refs.hackathon_signup_code.value;
                console.log(code);
                api.registerHackathon(code).then(() => {
                    this.messages.hackathon = "Successfully registered!";
                }, (error) => {
                    this.messages.hackathon = `Error: ${error.message || error.responseJSON.message}`;
                });
            },
        },
    }
</script>

<style lang="scss" scoped>
    .form-inline{
        .btn-ha{
            display: inline-block;
        }
    }
    .game-participant {
        img {
            height: 20px;
        }
    }
</style>

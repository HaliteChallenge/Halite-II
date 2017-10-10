<template>
    <div class="row">
        <div class="col-md-3">
            <div class="user-profile">
                <div class="user-profile-avatar">
                    <i class="xline xline-top"></i>
                    <img class="img-responsive" :src="'https://github.com/' + user.username + '.png'" :alt="user.username" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'">
                </div>
                <div class="user-profile-detail">
                    <a class="user-name" target="_blank" :href="'https://github.com/' + user.username">{{ user.username }}</a>
                    <a v-if="is_my_page" href="/user/edit-user"><i class="fa fa-pencil user-profile-edit-link"></i></a>
                    <p>{{ user.level }} <span v-if="user.organization">at <a  :href="`/programming-competition-leaderboard?organization=${user.organization_id}`">{{ user.organization }}</a></span></p>
                    <p v-if="user.location">
                      From <a :href="`/programming-competition-leaderboard?country=${user.country_code}`">{{user.location}}</a>
                    </p>
                    <p v-if="botLang.length > 0">Bots in
                        <template v-for="(lang, index) in botLang">
                            <span v-if="lang.length > 0" class="hl"><a  :href="`/programming-competition-leaderboard?language=${lang}`">{{lang}}</a></span><span v-if="(index+1) < botLang.length">,</span>
                        </template>
                    </p>
                </div>
                <div class="user-profile-rank">
                    <i class="xline xline-top"></i>
                    <h2><span :class="tierClass(user.tier || 'Salt')"></span> {{ user.rank ? `rank ${user.rank}` : "No Rank" }}, {{ user.tier || "Salt" }} tier</h2>
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
                    <p><a :href="`/programming-competition-leaderboard?show_user=${user.user_id}`">View on leaderboard</a></p>
                </div>
                <!-- <div class="user-profile-badge">
                    <i class="xline xline-top"></i>
                    <h2>Badges</h2>
                    <div class="user-profile-badge-page"><img v-on:click.stop.prevent="prev_badge" :src="`${baseUrl}/assets/images/page-prev.svg`"><img v-on:click.stop.prevent="next_badge" :src="`${baseUrl}/assets/images/page-next.svg`"></div>
                    <div class="user-profile-badge-content">
                        <ul class="user-profile-badge-list">
                            <li><img :src="`${baseUrl}/assets/images/temp/badge_1.png`"></li>
                            <li><img :src="`${baseUrl}/assets/images/temp/badge_2.png`"></li>
                            <li><img :src="`${baseUrl}/assets/images/temp/badge_3.png`"></li>
                            <li><img :src="`${baseUrl}/assets/images/temp/badge_4.png`"></li>
                            <li><img :src="`${baseUrl}/assets/images/temp/badge_5.png`"></li>
                        </ul>
                    </div>
                    <a v-if="is_my_page" class="user-profile-badge-button"><img :src="`${baseUrl}/assets/images/temp/add_profile.png`"></a>
                </div> -->
            </div>
        </div>
        <div class="col-md-7">
            <section class="profile-section">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Game Videos Feed
                </h2>

                <div v-if="!games.length" class="section-empty">
                    <img :src="`${baseUrl}/assets/images/temp/game_video.png`" class="icon-"></img>
                    <h2>Nothing to show</h2>
                    <p>Complete your first game and view replays<br/>here</p>
                </div>
                <div v-if="games.length">
                    <table class="table table-leader">
                        <thead>
                            <tr>
                                <th>Played On</th>
                                <th>Opponents</th>
                                <th>Map Size</th>
                                <th>Watch</th>
                                <th>Turns</th>
                                <th>Won</th>
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
                                    <a v-for="player in game.playerSorted"
                                       :href="'/user?user_id=' + player.id"
                                       class="game-participant"
                                       v-bind:class="{ 'timed-out': player.timed_out }"
                                       :title="player.timed_out ? 'This player timed out or errored in this game. See the log for details.' : ''">
                                        <img :alt="player" :src="profile_images[player.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" />
                                        <span class="rank">
                                            {{ player.rank }}
                                        </span>
                                    </a>
                                </td>
                                <td>{{ game.map_width }}x{{ game.map_height }}</td>
                                <td>
                                    <a :href="'/play?game_id=' + game.game_id">
                                        {{ game.game_id }}
                                    </a>
                                </td>
                                <td >
                                  {{ game.turns_total }}
                                </td>
                                <td class="winner">
                                  {{ game.players[user.user_id].rank === 1 ? 'Won' : '' }}
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
                </div>

            </section>
            <section class="profile-section">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Nemeses
                </h2>

                <div v-if="!nemesisList.length" class="section-empty">
                    <img :src="`${baseUrl}/assets/images/temp/game_video.png`" class="icon-"></img>
                    <h2>No nemeses yet</h2>
                    <p>Submit your first bot to uncover your nemeses<br/>here</p>
                </div>
                <div v-if="nemesisList.length > 0">
                    <div class="table-sticky-container">
                        <table class="table table-leader table-sticky">
                            <thead>
                                <tr>
                                    <th>Nemesis</th>
                                    <th>Games</th>
                                    <th>Win %</th>
                                    <th>Loss %</th>
                                </tr>
                            </thead>
                        </table>
                        <div class="table-scrollable-content">
                            <table class="table table-leader">
                                <thead>
                                    <tr>
                                       <th>Nemesis</th>
                                        <th>Games</th>
                                        <th>Win %</th>
                                        <th>Loss %</th>
                                    </tr>
                                </thead>
                                <tbody>
                                     <tr v-for="nemesis in nemesisList">
                                        <td>
                                            <a :href="'/user?user_id=' + nemesis.id"
                                            class="game-participant">
                                                <img :src="profile_images[nemesis.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" />
                                                <span class="rank">
                                                    {{nemesis.id}}
                                                </span>
                                            </a>
                                        </td>
                                        <td>
                                            {{nemesis.total}}
                                        </td>
                                        <td>
                                            {{nemesis.wins}}
                                        </td>
                                        <td>
                                            {{nemesis.losses}}
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>
            </section>
            <section v-if="is_my_page" class="profile-section profile-section-hackathon">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Your Hackathons
                </h2>

                <div v-if="!hackathons.length" class="section-empty">
                    <img :src="`${baseUrl}/assets/images/temp/event.png`" class="icon-"></img>
                    <h2>Your haven't join any event</h2>
                    <p>Start joining a Hackathon by adding code to<br/>your profile</p>
                    <div v-if="is_my_page" class="ha-button-container">
                        <div>
                            <a :href="`${baseUrl}/hackathon-and-events`" class="ha-button"><span>Join a Hackathon</span></a>
                        </div>
                    </div>
                </div>

                <form v-if="is_my_page" class="profile-section-right-form">
                    <div class="form-inline-button">
                        <input type="text" placeholder="Hackathon signup code" ref="hackathon_signup_code">
                        <button class="btn" v-on:click="join_hackathon"><span>Join Hackathon</span></button>
                    </div>
                </form>
                <div v-if="hackathons.length > 0">
                    <div class="table-sticky-container">
                        <table class="table table-leader table-sticky">
                            <thead>
                                <tr>
                                    <th>Hackathons</th>
                                    <th>Location</th>
                                    <th>Status</th>
                                    <th></th>
                                </tr>
                            </thead>
                        </table>
                        <div class="table-scrollable-content">
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
                                        <td>{{hackathon.status.charAt(0).toUpperCase() + hackathon.status.slice(1)}}</td>
                                        <td><a :href="'/hackathon-individual?hackathon_id=' + hackathon.hackathon_id">View Hackathon</a></td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>
            </section>
            <section v-if="is_my_page" class="profile-section profile-section-error">
                <h2>
                    <i class="xline xline-bottom"></i>
                    Your Errors
                </h2>
                <div>
                    <div class="table-sticky-container">
                        <table class="table table-leader table-sticky">
                            <thead>
                                <tr>
                                    <th>Id</th>
                                    <th>Reason</th>
                                    <th>Date</th>
                                    <th>Log File</th>
                                    <th>Game File</th>
                                </tr>
                            </thead>
                        </table>
                        <div class="table-scrollable-content">
                            <table class="table table-leader">
                                <thead>
                                    <tr>
                                        <th>Id</th>
                                        <th>Reason</th>
                                        <th>Date</th>
                                        <th>Log File</th>
                                        <th>Game File</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    <tr v-for="game in error_games">
                                        <td>{{game.game_id}}</td>
                                        <td>Time out</td>
                                        <td><time :datetime="game.time_played"
                                                  :title="game.time_played">
                                                {{ game.time_played | moment("calendar") }}
                                            </time></td>
                                        <td><a :href="error_log_link(game.game_id)" target="_blank">Download Log</a></td>
                                        <td><a :href="replay_link(game.game_id)" target="_blank">Download Game</a></td>
                                    </tr>
                                </tbody>
                            </table>
                        </div>
                    </div>
                </div>
            </section>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";
    import {Alert, tierClass} from "../utils.js";
    import Vue from "vue";
    import * as utils from "../utils";

    export default {
        name: "UserProfile",
        props: ['baseUrl'],
        data: function() {
            return {
                tierClass: tierClass,
                user: {
                    "level": "",
                    "username": "",
                    "organization": "",
                    "points": "",
                    "num_games": "",
                    "user_id": "",
                },
                games: [],
                nemesisList: [],
                bots: [],
                error_games: [],
                hackathons: [],
                profile_images: {},
                page: 0,
                limit: 10,
                offset: 0,
                nemesisLimit: 30,
                nemesisGameCount: 200,
                nemesisGameThreshold: 10,
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
                this.user.location = this.getLocation();

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
                this.fetchErrorGames();
                this.nemesis();
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
            setupStickyTable: function(){
                const calcCol = () => {
                    const el = $(this.$el).find('.table-sticky-container').each(function(){
                        const heading = $(this).find('.table-sticky th');
                        const body = $(this).find('.table:not(.table-sticky) th');
                        heading.each(function(index){
                            $(this).width($(body[index]).width());
                        });
                    });
                }
                setTimeout(() => {
                    calcCol();
                }, 500);
                $(window).on('resize', () => {
                    console.log('resize');
                    calcCol();
                });
            },
            fetch: function() {
                let query = `order_by=desc,time_played&offset=${this.offset}&limit=${this.limit}`;
                if (this.only_timed_out) {
                    query += `&filter=timed_out,=,${this.user.user_id}`;
                }
                const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`;
                return $.get(url).then((data) => {
                    this.games = data;
                    for (let game of data) {
                        for (let participant of Object.keys(game.players)) {
                            game.players[participant].id = participant;
                            if (this.profile_images[participant]) continue;
                            this.profile_images[participant] = "loading";

                            api.get_user(participant).then((user) => {
                                this.profile_images[participant] = api.make_profile_image_url(user.username);
                                this.$forceUpdate();
                            });
                        }

                        const players = Object.values(game.players).sort((r1, r2) => {
                          if (r1.id.toString() === this.user.user_id.toString())
                            return -1;
                          if (r2.id.toString() === this.user.user_id.toString())
                            return 1;
                          return r2.rank - r1.rank;
                        });

                        game.playerSorted = players;
                    }
                });
            },
            getLocation: function() {
              const user = this.user;
              let state = '', country = '';
              const countries = require("i18n-iso-countries");

              if (user.country_code) {
                const countryAlpha2 = countries.alpha3ToAlpha2(user.country_code);
                const countryData = iso3166.data[countryAlpha2];
                let stateData;
                if (countryData && user.country_subdivision_code) {
                  stateData = countryData.sub[user.country_subdivision_code]
                }
                state = stateData ? stateData.name : '';
                country = countryData ? countryData.name : '';
              }
              const location = `${state ? state + ', ' : ''}${country}`;
              return location ? location : '';
            },
            nemesis: function() {
                let query = `order_by=desc,time_played&offset=0&limit=${this.nemesisGameCount}`;
                const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`;
                return $.get(url).then((data) => {
                    var nemesisMap = new Map()
                    for (let game of data) {
                        if(game.players[this.user.user_id].rank === 1){
                            for (let participant of Object.keys(game.players)) {
                                if(participant == this.user.user_id){
                                    continue;
                                }

                                let playerData = nemesisMap.get(participant);
                                if(playerData){
                                    playerData.losses++;
                                }
                                else{
                                    let obj = {wins: 0, losses:1}
                                    nemesisMap.set(participant,obj);
                                }
                            }}
                        else{
                            for (let participant of Object.keys(game.players)) {
                                if(participant == this.user.user_id){
                                    continue;
                                }

                                if(game.players[participant].rank === 1){
                                    let playerData = nemesisMap.get(participant);
                                    if(playerData){
                                        playerData.wins++;
                                    }
                                    else{
                                        let obj = {wins: 1, losses:0}
                                        nemesisMap.set(participant,obj);
                                    }
                                }
                            }
                        }
                    }

                    for (var [key, value] of nemesisMap) {
                        let totalGames = value.wins + value.losses;
                        let winRatio = value.wins/totalGames;
                        let lossRatio = value.losses/totalGames;
                        if(totalGames >= this.nemesisGameThreshold){
                            var obj = {id:key, wins:Math.round(winRatio*100), losses:Math.round(lossRatio*100), total:totalGames};
                            this.nemesisList.push(obj);
                        }
                        api.get_user(key).then((user) => {
                                this.profile_images[key] = api.make_profile_image_url(user.username);
                                this.$forceUpdate();
                        });
                    }

                    this.nemesisList.sort(function(a,b) { return b.losses - a.losses});
                    this.nemesisList = this.nemesisList.slice(1, this.nemesisLimit);
                    console.log(this.nemesisList.length);
                    this.setupStickyTable();
                });
            },
            fetchHackathon: function(){
                api.getUserHackathons(this.user.user_id).then(hackathons => {
                    if(hackathons && hackathons instanceof Array) {
                        this.hackathons = hackathons.filter((h) => {
                            console.log(h);
                            return h.participant == true;
                        });
                    }
                });
            },
            fetchErrorGames: function(){
                let query = `order_by=desc,time_played&offset=0&limit=50&filter=timed_out,=,${this.user.user_id}`;
                const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`;
                return $.get(url).then((data) => {
                    this.error_games = data;
                    this.setupStickyTable();
                })
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

            replay_link: function(game_id) {
                return `${api.API_SERVER_URL}/user/${this.user.user_id}/match/${game_id}/replay`;
            },

            join_hackathon: function(event) {
                event.preventDefault();
                const code = this.$refs.hackathon_signup_code.value;
                gaData('hackathon','click-submit-hackathon-code','hackathon-flow');
                api.registerHackathon(code).then(() => {
                    Alert.show("Successfully registered!", 'success');
                    gaData('hackathon','hackathon-code-success','hackathon-flow');
                }, (error) => {
                    Alert.show(`Error: ${error.message || error.responseJSON.message}`);
                    gaData('hackathon','hackathon-code-error','hackathon-flow');
                });
            },
            prev_badge: ()=>{
                let content = $(".user-profile-badge-content");
                let list = $(".user-profile-badge-list");
                let contentWidth = $(content).width();
                let listWidth = $(list).children("li").outerWidth(true) * $(list).children("li").length;
                let marginLeft = parseInt($(list).css('marginLeft'));
                let interval = 20;
                let aniVal = 0;
                let cal = listWidth + marginLeft - contentWidth;
                if(cal > interval){
                    aniVal = interval;
                }else if(0 < cal <= interval){
                    aniVal = cal;
                }else{
                    aniVal = 0;
                }
                $(list).animate({marginLeft:'-='+aniVal+'px'});
            },
            next_badge: ()=>{
                let list = $(".user-profile-badge-list");
                let marginLeft = Math.abs(parseInt($(list).css('marginLeft')));
                let interval = 20;
                let aniVal = 0;
                if(marginLeft > interval){
                    aniVal = interval;
                }else if(0 < marginLeft <= interval){
                    aniVal = marginLeft;
                }else{
                    aniVal = 0;
                }
                $(list).animate({marginLeft:'+='+aniVal+'px'});
            },
            gaData: function(category, action, label) {
                utils.gaEvent(category, action, label);
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

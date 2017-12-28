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
                    <p>{{ user.level }} <span v-if="user.organization">at <a  :href="`/programming-competition-leaderboard?organization=${user.organization_id}`">{{ user.organization }}</a></span></p>
                    <p v-if="user.location">
                      From <a :href="`/programming-competition-leaderboard?country=${user.country_code}`">{{user.location}}</a>
                    </p>
                    <p v-if="botLang.length > 0">Bots in
                        <template v-for="(lang, index) in botLang">
                            <span v-if="lang.length > 0" class="hl"><a  :href="`/programming-competition-leaderboard?language=${lang}`">{{lang}}</a></span><span v-if="(index+1) < botLang.length">,</span>
                        </template>
                    </p>
                    <div v-if="is_my_page && bots && bots[0] && bots[0].compilation_status==='Disabled'" class="text-center" style="margin-top: 10px;">
                        <p class="warning">
                            Your bot is disabled   <span title="Due to excessive timeouts or errors, you bot has been disabled, look at the game logs to debug the issue or try submitting it again." class="info-icon icon-info"></span>
                        </p>
                    </div>
                     <div v-if="is_my_page && bots && bots[0] && bots[0].compilation_status==='Failed'" class="text-center" style="margin-top: 10px;">
                        <p class="warning">
                            Your bot failed to compile   <span title="Look at the compilation failure mail to debug the issue or try submitting it again." class="info-icon icon-info"></span>
                        </p>
                    </div>
                </div>
                <div class="user-profile-rank">
                    <i class="xline xline-top"></i>
                    <h2><span :class="tierClass(user.tier || 'Bronze')"></span> {{ user.rank ? `rank ${user.rank}` : "No Rank" }}, {{ user.tier || "Bronze" }} tier</h2>
                    <div class="user-profile-rank-stats">
                        <div class="stats-item">
                            <h3>Rating</h3>
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
                    <h2 class="highest-rank" v-if="highestRank" title="This is either your current (top rank) or highest rank any of your bots had achieved when retired"> Highest Rank Achieved: {{highestRank}}</h2>

                </div>
                <div class="game-replay-share text-center">
                    <div class="popup-overlay" v-show="sharePopup" @click="toggleShare"></div>
                    <div class="popup-container" v-show="sharePopup">
                        <div class="popup-share">
                            <label>Share as a link</label>
                            <div class="form-inline-button">
                                <input ref="shareInput" type="text" :value="shareLink">
                                <button class="btn" @click="copyToClipboard">
                                    <span>Copy</span>
                                </button>
                            </div>
                            <div class="share-socials">
                                <a :href="shareSocial('facebook')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"><i class="fa fa-facebook-official"></i></a>
                                <a :href="shareSocial('twitter')"><i class="fa fa-twitter"></i></a>
                                <a :href="shareSocial('linkedin')" onclick="javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;" target="_blank"><i class="fa fa-linkedin"></i></a>
                            </div>
                        </div>
                    </div>
                    <div>
                        <button class="btn" @click="openChallengeModal">
                            <span>CHALLENGE</span>
                        </button>
                        <ChallengeModal v-if="is_my_page" :baseUrl="baseUrl" :isOn="isChallengeModalOpen" :close="closeChallengeModal" username=""></ChallengeModal>  
                        <ChallengeModal v-else :baseUrl="baseUrl" :isOn="isChallengeModalOpen" :close="closeChallengeModal" :username="user.username"></ChallengeModal>  
                        <!-- <button class="btn" @click="toggleShare">
                            <span>SHARE</span>
                        </button> -->
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
                        <p v-if="userHistory.length" style="margin-top: 20px;"><a :href="`/programming-competition-leaderboard?show_user=${user.user_id}`">View on leaderboard</a></p>
                    </div>
                </div>
                <div class="stats-1-section">
                    <i class="xline xline-top"></i>
                    <h2 v-if="season1stats && season1stats.num_submissions > 0">Halite 1 Stats</h2>
                    <div v-if="season1stats && season1stats.num_submissions > 0" class="user-profile-rank-stats">
                        <div class="stats-item">
                            <h3>Rank</h3>
                            <p>{{ season1stats.rank }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Bots</h3>
                            <p>{{season1stats.num_submissions }}</p>
                        </div>
                        <div class="stats-item">
                            <h3>Games</h3>
                            <p>{{ season1stats.num_games }}</p>
                        </div>
                    </div>
                    <p class="text-center">
                        <a v-if="season1stats && season1stats.num_submissions > 0" class="user-name" target="_blank" :href="'https://2016.halite.io/user.php?userID=' + season1stats.userID">View Halite 1 Profile</a>
                    </p>
                </div>
            </div>
        </div>
        <div class="col-md-7">
            <div class="user-widget tab-container">
                <ul class="nav nav-tabs" role="tablist">
                    <li role="presentation" class="active">
                        <a href="#games" @click="refreshStickyTable" aria-controls="games" role="tab" data-toggle="tab">
                        <i class="xline xline-top"></i>
                        <span>Games & Logs</span>
                        <i class="xline xline-bottom"></i>
                        </a>
                    </li>
                    <li role="presentation">
                        <a href="#analysis" @click="refreshStickyTable" aria-controls="analysis" role="tab" data-toggle="tab">
                        <i class="xline xline-top"></i>
                        <span>Analysis</span>
                        <i class="xline xline-bottom"></i>
                        </a>
                    </li>
                    <li role="presentation">
                        <a href="#hackathons" @click="refreshStickyTable" aria-controls="hackathons" role="tab" data-toggle="tab">
                        <i class="xline xline-top"></i>
                        <span>Hackathons</span>
                        <i class="xline xline-bottom"></i>
                        </a>
                    </li>
                </ul>
                <!-- Tab panes -->
                <div class="tab-content">
                    <div role="tabpanel" class="tab-pane active" id="games">
                        <div id="games_pane">
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Game Videos Feed
                                    <span title="Games played by your bot, replay files are kept forever, but games data might be deleted every 2 weeks" class="info-icon icon-info pull-right"></span>
                                </h2>

                                <div v-if="!games.length" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/temp/game_video.png`" class="icon-"></img>
                                    <h2>No games played yet</h2>
                                    <p v-if="is_my_page">Complete your first game and view replays. <br/> <a href="/play-programming-challenge">Play here</a></p>
                                </div>
                                <div v-if="games.length">
                                    <table class="table table-leader">
                                        <thead>
                                            <tr>
                                                <th>Watch</th>
                                                <th style="padding-left: 40px;">Result</th>
                                                <th class="text-center hidden-xs" >Map Size</th>
                                                <th class="text-center hidden-xs">Turns</th>
                                            </tr>
                                        </thead>
                                        <tbody>
                                            <tr v-for="game in games">
                                                <td v-bind:class="game.versions_back ? (game.versions_back % 2 ? 'old-bot-odd' : 'old-bot-even') : ''">
                                                    <a :href="'/play?game_id=' + game.game_id">
                                                        {{getFormattedDateForGames(game.time_played)}}
                                                    </a>
                                                </td>
                                                <td v-bind:class="{ 'challenge': game.challenge_id }">
                                                    <div class="info-icon-trophy" v-if="game.players[user.user_id].rank === 1">
                                                        <span class="icon-trophy"></span>
                                                    </div>
                                                    <a v-for="player in game.playerSorted"
                                                    :href="'/user?user_id=' + player.id"
                                                    class="game-participant"
                                                    :title="player.name_rank + (player.timed_out ? ' timed out or errored in this game. See the log for details.' : '')">
                                                        <img :alt="player" :src="profile_images[player.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" v-bind:class="{ 'timed-out': player.timed_out }"/>
                                                        <span class="rank">
                                                            {{ player.rank }}
                                                        </span>
                                                    </a>
                                                </td>
                                                <td class="text-center hidden-xs">{{ game.map_width }}x{{ game.map_height }}</td>
                                                <td class="text-center hidden-xs">
                                                {{ game.turns_total }}
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
                                            :disabled="isLastPage"
                                            v-on:click="next_page"><span>Next</span></button>
                                    </div>
                                </div>

                            </section>
                            <section v-if="is_my_page" class="profile-section profile-section-error">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Your Errors
                                    <span title="Download the replay files and error logs (last 30) for games where your bot errored or timed out." class="info-icon icon-info pull-right"></span>
                                </h2>
                                 <div v-if="!error_games.length" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/leaderboard-zero-icon.png`" class="icon-"></img>
                                    <h2>No errors yet</h2>
                                </div>
                                <div>
                                    <div v-if="error_games.length > 0" class="table-sticky-container">
                                        <div class="table-wrapper">
                                            <table class="table table-leader table-sticky">
                                                <thead>
                                                    <tr>
                                                        <th>Id</th>
                                                        <th class="hidden-xs">Date</th>
                                                        <th>Log File</th>
                                                        <th>Game</th>
                                                    </tr>
                                                </thead>
                                            </table>
                                            <div class="table-scrollable-content">
                                                <table class="table table-leader">
                                                    <thead>
                                                        <tr>
                                                            <th>Id</th>
                                                            <th class="hidden-xs">Date</th>
                                                            <th>Log File</th>
                                                            <th>Game</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        <tr v-for="game in error_games">
                                                            <td>{{game.game_id}}</td>
                                                            <td class="hidden-xs"><time :datetime="game.time_played"
                                                                    :title="game.time_played">
                                                                    {{ getFormattedDateForGames(game.time_played)}}
                                                                </time>
                                                            </td>
                                                            <td><a :href="error_log_link(game.game_id)" target="_blank">Download Log</a></td>
                                                            <td><a :href="replay_link(game.game_id)" target="_blank">View</a> / <a :href="replay_download_link(game.game_id)" target="_blank">Download</a></td>
                                                        </tr>
                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </section>
                        </div>
                    </div>
                    <div role="tabpanel" class="tab-pane" id="analysis">
                        <div id="map_stats_pane">
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Rating Analysis
                                    <span title="Rating is calculated as mu - 3 * sigma;" class="info-icon icon-info pull-right"></span>
                                </h2>
                                <div v-if="!user.mu" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/leaderboard-zero-icon.png`" class="icon-"></img>
                                    <h2>No rating analysis</h2>
                                    <p v-if="is_my_page">Submit your first bot to get your rating. <br/> <a href="/play-programming-challenge">Play here</a></p>
                                </div>
                                <div v-if="user.mu" class="user-profile-rank-stats">
                                    <div class="stats-item">
                                        <h3>Rating</h3>
                                        <p>{{ Math.round(user.score * 100) / 100 }}</p>
                                    </div>
                                    <div class="stats-item">
                                        <h3>&mu;</h3>
                                        <p>{{ Math.round(user.mu * 100) / 100 }}</p>
                                    </div>
                                    <div class="stats-item">
                                        <h3>&sigma;</h3>
                                        <p>{{ Math.round(user.sigma * 100) / 100 }}</p>
                                    </div>
                                </div>
                            </section>
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Nemesis
                                    <span title="Players you most often lose/win (minimum 10 games played) against, based on analysis of the last 200 games." class="info-icon icon-info pull-right"></span>
                                </h2>
                                <div v-if="!nemesisList.length" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/leaderboard-zero-icon.png`" class="icon-"></img>
                                    <h2>No nemesis yet</h2>
                                    <p v-if="is_my_page">Submit your first bot to uncover your nemesis. <br/><a href="/play-programming-challenge">Play here</a></p>
                                </div>
                                <div v-if="nemesisList.length > 0">
                                    <div class="table-sticky-container">
                                        <div class="table-wrapper">
                                            <table class="table table-leader table-sticky">
                                                <thead>
                                                    <tr>
                                                        <th>Nemesis</th>
                                                        <th class="text-center hidden-xs">Games</th>
                                                        <th class="text-center">Win %</th>
                                                        <th class="text-center">Loss %</th>
                                                    </tr>
                                                </thead>
                                            </table>
                                            <div class="table-scrollable-content">
                                                <table class="table table-leader">
                                                    <thead>
                                                        <tr>
                                                            <th>Nemesis</th>
                                                            <th class="text-center hidden-xs">Games</th>
                                                            <th class="text-center">Win %</th>
                                                            <th class="text-center">Loss %</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        <tr v-for="nemesis in nemesisList">
                                                            <td>
                                                                <a :href="'/user?user_id=' + nemesis.id"
                                                                class="game-participant">
                                                                    <img :src="profile_images[nemesis.id]" onerror="this.src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Placeholder_no_text.svg/2000px-Placeholder_no_text.svg.png'" />
                                                                    <span class="rank">
                                                                        {{usernames[nemesis.id]}}
                                                                    </span>
                                                                </a>
                                                            </td>
                                                            <td class="text-center hidden-xs">
                                                                {{nemesis.total}}
                                                            </td>
                                                            <td class="text-center">
                                                                {{nemesis.wins}}
                                                            </td>
                                                            <td class="text-center">
                                                                {{nemesis.losses}}
                                                            </td>
                                                        </tr>
                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </section>
                            <section class="profile-section">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    History
                                    <span title="Rank/Rating history of your bots, the rank/rating is the last rating or rank achieved before the bot was retired." class="info-icon icon-info pull-right"></span>
                                </h2>
                                <div v-if="!userHistory.length" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/leaderboard-zero-icon.png`" class="icon-"></img>
                                    <h2>No history</h2>
                                    <p v-if="is_my_page">Submit your first bot to see your history.<br/> <a href="/play-programming-challenge">Play here</a></p>
                                </div>
                                <div v-if="userHistory.length > 0">
                                    <div class="table-sticky-container">
                                        <div class="table-wrapper">
                                            <table class="table table-leader table-sticky">
                                                <thead>
                                                    <tr>
                                                        <th>Bot Version</th>
                                                        <th class="text-center">Rating</th>
                                                        <th class="text-center">Rank</th>
                                                        <th class="text-center hidden-xs">Games</th>
                                                        <th class="hidden-xs">Retired On</th>
                                                    </tr>
                                                </thead>
                                            </table>
                                            <div class="table-scrollable-content">
                                                <table class="table table-leader">
                                                    <thead>
                                                        <tr>
                                                            <th>Bot Version</th>
                                                            <th class="text-center">Rating</th>
                                                            <th class="text-center">Rank</th>
                                                            <th class="text-center hidden-xs">Games</th>
                                                            <th class="hidden-xs">Retired On</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        <tr v-for="historyItem in userHistory">
                                                            <td>
                                                                {{historyItem.bot_version}}
                                                            </td>
                                                            <td class="text-center">
                                                                {{ Math.round(100 * historyItem.last_score) / 100 }}
                                                            </td>
                                                            <td class="text-center">
                                                                {{historyItem.last_rank}}
                                                            </td>
                                                            <td class="text-center hidden-xs">
                                                                {{historyItem.last_games_played}}
                                                            </td>
                                                            <td class="hidden-xs">
                                                                {{getFormattedDateForGames(historyItem.when_retired, "Still Playing")}}
                                                            </td>
                                                        </tr>
                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </section>
                        </div>
                    </div>
                    <div role="tabpanel" class="tab-pane" id="hackathons">
                        <div id="map_stats_pane">
                            <section class="profile-section profile-section-hackathon">
                                <h2>
                                    <i class="xline xline-bottom"></i>
                                    Hackathons
                                    <span title="All the hackathons in which you are as participant" class="info-icon icon-info pull-right"></span>
                                </h2>

                                <div v-if="!hackathons.length" class="section-empty">
                                    <img :src="`${baseUrl}/assets/images/temp/event.png`" class="icon-"></img>
                                    <h2>Not part of any Hackathons yet</h2>
                                    <p v-if="is_my_page">If you have a Hackthon code, add it to your <br/>your profile</p>
                                    <div v-if="is_my_page" class="ha-button-container">
                                        <div>
                                            <a :href="`${baseUrl}/user/edit-user`" class="ha-button"><span>Add your Hackathon code</span></a>
                                        </div>
                                    </div>
                                </div>
                                <div v-if="hackathons.length > 0">
                                    <div class="table-sticky-container">
                                        <div class="table-wrapper">
                                            <table class="table table-leader table-sticky">
                                                <thead>
                                                    <tr>
                                                        <th>Hackathon</th>
                                                        <th>Location</th>
                                                        <th>Status</th>
                                                    </tr>
                                                </thead>
                                            </table>
                                            <div class="table-scrollable-content">
                                                <table class="table table-leader">
                                                    <thead>
                                                        <tr>
                                                            <th>Hackathon</th>
                                                            <th>Location</th>
                                                            <th>Status</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        <tr v-for="hackathon in hackathons">
                                                            <td><a :href="'/hackathon-individual?hackathon_id=' + hackathon.hackathon_id">{{hackathon.title}}</a></td>
                                                            <td>{{hackathon.location}}</td>
                                                            <td>{{hackathon.status.charAt(0).toUpperCase() + hackathon.status.slice(1)}}</td>
                                                        </tr>
                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </section>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>
    import * as api from '../api'
    import {Alert, tierClass} from '../utils.js'
    import Vue from 'vue'
    import * as utils from '../utils'
    import moment from 'moment'
    import dateformat from 'dateformat'
    import ChallengeModal from './ChallengeModal.vue'

    export default {
      name: 'UserProfile',
      props: ['baseUrl'],
      components: {ChallengeModal},
      data: function () {
        return {
          tierClass: tierClass,
          user: {
            'level': '',
            'username': '',
            'organization': '',
            'points': '',
            'num_games': '',
            'user_id': ''
          },
          games: [],
          nemesisList: [],
          bots: [],
          error_games: [],
          hackathons: [],
          userHistory: [],
          profile_images: {},
          usernames: {},
          page: 0,
          limit: 10,
          offset: 0,
          nemesisLimit: 30,
          nemesisGameCount: 200,
          nemesisGameThreshold: 10,
          only_timed_out: false,
          is_my_page: false,
          highestRank: null,
          sharePopup: false,
          season1stats: null,
          messages: {
            hackathon: ''
          },
          isLastPage: false,
          isChallengeModalOpen: false,
        }
      },
      mounted: function () {
        const params = new URLSearchParams(window.location.search)
        let source

        if (params.has('me')) {
          source = api.me()
        } else {
          const user_id = params.get('user_id')
          source = api.get_user(user_id)
        }

        source.then((user) => {
          if (user === null) {
            window.location.replace(`${api.LOGIN_SERVER_URL}/github`)
            return
          }
          this.user = user
          this.user.location = this.getLocation()

          if (params.has('me')) {
            window.history.replaceState(
              {}, '',
              `${window.location.origin}${window.location.pathname}?user_id=${user.user_id}`)
          }
          api.list_bots(user.user_id).then((bots) => {
            this.bots = bots
          })
          this.fetch()
          this.fetchHackathon()
          this.fetchErrorGames()
          this.fetchnemesis()
          this.fetchhistory()
          this.fetchHalite1Stats()

          // switch to analysis tab if requested
          const url = new URLSearchParams(location.search)
          const view = url.get("view")
          console.log(view)
          if (view == 'analysis'){
            $('a[href="#analysis"]').trigger('click')
            this.setupStickyTable()
          }
        }, (e) => {
          window.location.replace(`${this.baseUrl}/404`);
        })

        api.me().then((me) => {
          this.is_my_page = me && me.user_id === this.user.user_id
        })

        // sticky tables
        this.setupStickyTable()
      },
      computed: {
        botLang: function () {
          let lang = []
          if (this.bots.length > 0) {
            for (let i = 0; i < this.bots.length; i++) {
              if (lang.indexOf(this.bots[i].language) == -1) {
                lang.push(this.bots[i].language)
              }
            }
          }
          return lang
        },
        shareLink: function () {
          return window.location.href
        }
      },
      methods: {
        setupStickyTable: function () {
          $(window).on('resize', () => {
            calcCol()
          })
        },
        refreshStickyTable: function () {
            window.refreshStickyTable();
        },
        fetch: function () {
          let query = `order_by=desc,time_played&offset=${this.offset}&limit=${this.limit}`
          if (this.only_timed_out) {
            query += `&filter=timed_out,=,${this.user.user_id}`
          }
          const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`

          return new Promise((resolve, reject) => {
            $.get(url).then((data) => {
                console.log(data);
                console.log(data.length);
                if ( data.length > 0 ){
                    this.games = data
                    for (let game of data) {
                      for (let player_id in game.players) {
                        let player = game.players[player_id]
                        let username = player.username
                        let rating = player.mu - (player.sigma * 3)
                        player.rating = rating
                        rating = Math.round(rating * 100) / 100
                        let mu = Math.round(player.mu * 100) / 100
                        let sigma = Math.round(player.sigma * 1000) / 1000

                        player.id = player_id
                        player.name_rank = `(${player.leaderboard_rank}) ${username} [${rating}=${mu}μ${sigma}σ]`

                        this.profile_images[player_id] = api.make_profile_image_url(username)
                        this.usernames[player_id] = username

                        if (player_id == this.user.user_id) {
                            game.versions_back = this.user.num_submissions - player.version_number
                        }
                      }

                      const players = Object.values(game.players).sort((r1, r2) => {
                        if (r1.id.toString() === this.user.user_id.toString()) { return -1 }
                        if (r2.id.toString() === this.user.user_id.toString()) { return 1 }
                        return r1.rank - r2.rank
                      })

                      game.playerSorted = players
                    }

                    resolve(data)
                } else {
                    reject("Last page reached")
                }
              })
          });
        },
        getLocation: function () {
          const user = this.user
          let state = '', country = ''
          const countries = require('i18n-iso-countries')

          if (user.country_code) {
            const countryAlpha2 = countries.alpha3ToAlpha2(user.country_code)
            const countryData = iso3166.data[countryAlpha2]
            let stateData
            if (countryData && user.country_subdivision_code) {
              stateData = countryData.sub[user.country_subdivision_code]
            }
            state = stateData ? stateData.name : ''
            country = countryData ? countryData.name : ''
          }
          const location = `${state ? state + ', ' : ''}${country}`
          return location || ''
        },
        fetchnemesis: function () {
          let query = `order_by=desc,time_played&offset=0&limit=${this.nemesisGameCount}`
          const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`
          return $.get(url).then((data) => {
            var nemesisMap = new Map()
            for (let game of data) {
              let user_player = game.players[this.user.user_id]
              for (let participant of Object.keys(game.players)) {
                if (participant == this.user.user_id) {
                  continue
                }

                let username = game.players[participant].username
                this.profile_images[participant] = api.make_profile_image_url(username)
                this.usernames[participant] = username

                let playerData = nemesisMap.get(participant)
                if (typeof playerData === 'undefined') {
                  playerData = {wins: 0, losses: 0}
                  nemesisMap.set(participant, playerData)
                }

                if (user_player.rank < game.players[participant].rank) {
                  playerData.wins++
                } else {
                  playerData.losses++
                }
              }
            }
            for (var [key, value] of nemesisMap) {
              let totalGames = value.wins + value.losses
              let winRatio = value.wins / totalGames
              let lossRatio = value.losses / totalGames
              if (totalGames >= this.nemesisGameThreshold) {
                var obj = {
                  id: key,
                  wins: Math.round(winRatio * 100),
                  losses: Math.round(lossRatio * 100),
                  total: totalGames
                }
                this.nemesisList.push(obj)
              }
            }

            this.nemesisList.sort(function (a, b) { return b.losses - a.losses })
            this.nemesisList = this.nemesisList.slice(1, this.nemesisLimit)
            this.refreshStickyTable()
          })
        },
        fetchHalite1Stats: function () {
          api.get_season1_stats(this.user.user_id).then(userDetails => {
            this.season1stats = userDetails;
            console.log(this.season1stats);
          })
        },
        fetchHackathon: function () {
          api.getUserHackathons(this.user.user_id).then(hackathons => {
            if (hackathons && hackathons instanceof Array) {
              this.hackathons = hackathons.filter((h) => {
                return h.participant == true
              })
            }
          })
        },
        fetchhistory: function () {
          api.getUserHistory(this.user.user_id).then(history => {
            if (history && history instanceof Array) {
              history.sort(function (a, b) { return parseInt(b.bot_version) - parseInt(a.bot_version) })
              this.userHistory = history
              if (this.user.num_submissions > 0) {
                this.userHistory.unshift({bot_version: 'Current (' + this.user.num_submissions + ')', last_score: this.user.score, last_rank: this.user.rank, last_games_played: this.user.num_games, when_retired: 'Still playing' })
              }
              if (history.length <= 0) {
                return
              }
              this.highestRank = history.reduce((min, p) => p.last_rank < min ? p.last_rank : min, history[0].last_rank)
              if (this.highestRank > this.user.rank) {
                this.highestRank = this.user.rank
              }
            }
          })
        },
        fetchErrorGames: function () {
          let query = `order_by=desc,time_played&offset=0&limit=50&filter=timed_out,=,${this.user.user_id}`
          const url = `${api.API_SERVER_URL}/user/${this.user.user_id}/match?${query}`
          return $.get(url).then((data) => {
            this.error_games = data
            this.refreshStickyTable()
          })
        },
        next_page: function () {
          this.offset += 10
          this.fetch().then((data) => {
            this.page += 1
            this.isLastPage = false;
          }).catch((message) => {
            this.isLastPage = true;
          })
        },
        prev_page: function () {
          this.offset -= 10
          this.fetch().then(() => {
            this.page -= 1
          })
        },
        toggle_filter: function () {
          this.only_timed_out = !this.only_timed_out
          this.offset = 0
          this.fetch().then(() => {
            this.page = 0
          })
        },
        error_log_link: function (game_id) {
          return `${api.API_SERVER_URL}/user/${this.user.user_id}/match/${game_id}/error_log`
        },
        replay_download_link: function (game_id) {
          return `${api.API_SERVER_URL}/user/${this.user.user_id}/match/${game_id}/replay`
        },
        replay_link: function (game_id) {
          return `/play/?game_id=${game_id}`
        },
        prev_badge: () => {
          let content = $('.user-profile-badge-content')
          let list = $('.user-profile-badge-list')
          let contentWidth = $(content).width()
          let listWidth = $(list).children('li').outerWidth(true) * $(list).children('li').length
          let marginLeft = parseInt($(list).css('marginLeft'))
          let interval = 20
          let aniVal = 0
          let cal = listWidth + marginLeft - contentWidth
          if (cal > interval) {
            aniVal = interval
          } else if (cal > 0 <= interval) {
            aniVal = cal
          } else {
            aniVal = 0
          }
          $(list).animate({marginLeft: '-=' + aniVal + 'px'})
        },
        next_badge: () => {
          let list = $('.user-profile-badge-list')
          let marginLeft = Math.abs(parseInt($(list).css('marginLeft')))
          let interval = 20
          let aniVal = 0
          if (marginLeft > interval) {
            aniVal = interval
          } else if (marginLeft > 0 <= interval) {
            aniVal = marginLeft
          } else {
            aniVal = 0
          }
          $(list).animate({marginLeft: '+=' + aniVal + 'px'})
        },
        getFormattedDateForGames: function (date, return_value_not_valid) {
          var cdate = moment(date)
          if (cdate.isValid()) {
            var dateFormat = require('dateformat')
            return dateFormat(date, 'dd/mm/yy HH:MM')
          } else {
            return return_value_not_valid
          }
        },
        gaData: function (category, action, label) {
          utils.gaEvent(category, action, label)
        },
        toggleShare: function () {
          this.sharePopup = !this.sharePopup
        },
        shareSocial: function (social) {
          let text = 'Halite II Player - ' + this.user.username + ' Rank: ' + this.user.rank + ' Tier: ' + this.user.tier + ' '
          let tags = 'haliteplayerstats'
          switch (social) {
            case 'facebook':
              return 'https://www.facebook.com/sharer.php?u=' + encodeURIComponent(window.location.href)
              break
            case 'twitter':
              return 'https://twitter.com/intent/tweet?text=' + text + '&url=' + encodeURIComponent(window.location.href) + '&hashtags=' + tags + '&via=haliteAI'
              break
            case 'linkedin':
              return `https://www.linkedin.com/shareArticle?mini=true&url=${encodeURIComponent(window.location.href)}`
              break
          }
        },
        /**
             * @param  {e} event
             * @return {void}
             */
        copyToClipboard: function (e) {
          if (e) e.preventDefault()
          this.$refs.shareInput.select()
          document.execCommand('copy')
        },
        openChallengeModal: function(e) {
            this.isChallengeModalOpen = true;
        },
        closeChallengeModal: function(e) {
            this.isChallengeModalOpen = false;
        }
      }
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

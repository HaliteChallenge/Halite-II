<template>
  <div class="challenger-modal" :class="{'on': isOn}">
    <div class="modal-overlay" @click="close"></div>
    <div class="modal-container">
      <a @click="close" class="close modal-close"><span class="icon-remove"></span></a>
      <div class="send-challenge-form" v-if="!showResult">
        <img class="ico" :src="`${baseUrl}/assets/images/icon-group.svg`">
        <p data-v-48c46ce0="">The 2017-2018 season of Halite has ended. You can no longer challenge a player through our website. But you can always play Halite locally - read <a href="/learn-programming-challenge/halite-cli-and-tools/">our documentation</a> to learn more.</p>
        <!-- <h2 class="heading">CHALLENGE OTHER PLAYERS</h2>
        <p>You can choose one or three players from the leaderboard to challenge. You’ll be able to see how you stack up and we’ll let you know once the challenge is over.</p>
        <div v-if="me">
          <div class="user-search">
            <div class="challenge-list">
              <div class="search-item" v-for="(friend, index) in friends">
                <div v-if="!friend">
                  <div class="select-item" :class="{empty: validated && !emptyFields[index]}">
                    <v-select
                      placeholder="Select User"
                      v-model="friends[index]"
                      :options="options">
                    </v-select>
                  </div>
                </div>
                <div v-else-if="members[friends[index]]">
                  <div class="selected-friend">
                    <a :href="'/user?user_id=' + members[friends[index]].user_id"><img width="36" height="36" :src="`https://github.com/${friends[index]}.png`" alt=""> {{friends[index]}}</a>
                    <a class="close" @click="removeFriend(index)"><span class="icon-remove"></span></a>
                  </div>
                </div>
              </div>
            </div>
            <div>
              <a @click="addOpponent" class="add-more" v-if="friends.length < 3">
                <span><img :src="`${baseUrl}/assets/images/icon-add.svg`" alt="">
                  Add more opponents</span>
              </a>
            </div>
          </div>

          <div class="error-message" v-if="errorMessage && validated">
            {{this.errorMessage}}
          </div>
          <div class="ha-button-container">
            <div>
              <a class="ha-button" @click="submit"><span>SEND CHALLENGE</span></a>
            </div>
          </div>
        </div>
        <div class="require-login" v-else>
          <div class="no-login">You must sign in to challenge a player</div>
          <div class="ha-button-container">
            <div>
              <a class="ha-button" href="https://api.halite.io/v1/login/github" onclick="javascript:handleOutboundLinkClicks('account', 'click-to-sign-up','navigation');return true;"><span>SIGN INTO HALITE</span></a>
            </div>
          </div>
        </div> -->
      </div>
      <div class="send-challenge-result" v-else>
        <img class="ico" :src="`${baseUrl}/assets/images/icon-success.svg`">
        <h2 class="heading">SUCCESS!</h2>
        <p>We’ll let you know when your challenge is complete.</p>
        <div class="ha-button-container">
          <div>
            <a class="ha-button" :href="`${baseUrl}/user/?user_id=${me.user_id}&view=analysis`"><span>VIEW YOUR RESULTS</span></a>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import vSelect from 'vue-select'
import * as api from '../api'

export default{
  name: 'ChallengeModal',
  props: ['isOn', 'close', 'baseUrl', 'username'],
  components: {vSelect},
  data: function(){
    return {
      selectedUser: '',
      options: [],
      friends: [],
      members: {},
      me: false,
      errorMessage: "",
      emptyFields: [],
      validated: false,
      showResult: false,
    }
  },
  mounted: function(){
    api.me().then((me) => {
      if (me){
        this.me = me

        api.leaderboard(null, null, 0, 99999).then((members) => {
          this.options = members.map((member) => member.username )
            .filter((username) => username !== me.username)

          let arr = {};
          members.forEach((item) => {
            arr[item.username] = item
          });
          this.members = arr;
        })
      }
    });

    let options = []
    let members = {}

    if (this.username){
      this.friends.push(this.username)
    } else {
      this.friends.push("")
    }

  },
  watch: {
    username: function(newUsername){
      if (newUsername){
        this.friends[0] = newUsername
      } else {
        this.friends[0] = ""
      }
    },
    friends: function(newFriends) {
      let options = this.options;
      newFriends.forEach((username) => {
        options = options.filter((searchedUsername) => searchedUsername != username)
      })
      this.options = options
      this.validated = false
    },
    isOn: function(value){
      if (value){
        this.friends = this.username ? [this.username] : [""]
        this.errorMessage = ""
        this.emptyFields = []
        this.validated = false
        this.showResult = false
      }
    }
  },
  methods: {
    addOpponent: function(){
      this.validated = false
      while (this.friends.length < 3){
        this.friends.push("");
      }
    },
    removeFriend: function(index){
      this.validated = false
      this.friends[index] = ""
      this.$forceUpdate()
    },
    onSelectOpponent: function(index){
      let options = this.options;
      this.friends.forEach((username) => {
        options = options.filter((searchedUsername) => searchedUsername != username)
      })
      this.options = options
    },
    hasEmptyField: function(index){
      return this.emptyFields.indexOf(index) == -1
    },
    submit: function(){
      let emptyFields = {}
      let hasError = false
      try {
        // reset
        this.errorMessage = ""
        this.validated = false

        let emptyCount = 0;
        this.friends.forEach((item, index) => {
          if (!item){
            emptyFields[index] = 0
            hasError = true
            emptyCount++;
          } else {
            emptyFields[index] = 1
          }
        });

        if (emptyCount == 0 || this.friends.length - emptyCount == 1){
          let opponents = this.friends.filter((username) => username != "").map((username) => {
            return this.members[username].user_id
          })
          api.challenge(this.me.user_id, opponents).then((data) => {
            this.showResult = true;
          }, (error) => {
            this.showResult = false;
            throw error.responseJSON.message;
          })
        } else {
          this.emptyFields = emptyFields
          throw emptyCount == this.friends.length ? "Please select at least one player to challenge" : "Please select one or three players to challenge"
        }
      } catch (err) {
        console.log(err)
        this.validated = true // enable showing error
        this.errorMessage = err
      }
    }
  }
}
</script>
<style lang="scss" scoped>
  .challenger-modal{
    .modal-container{
      position: relative;
    }
    .modal-close{
      position: absolute;
      top: 10px;
      right: 10px;
      color: #BDCCC9;
      font-size: 12px;
      opacity: 1;
      text-shadow: none;
      text-decoration: none;
    }
  }
  .no-login{
    text-align: center;
    border-top: 1px solid #363347;
    border-bottom: 1px solid #363347;
    padding: 20px 0;
    margin: 20px 0;
    color: #63CECA;
  }
  .user-search{
    margin-bottom: 40px;
  }
  .challenge-list{
    border-top: 1px solid #363347;
    border-bottom: 1px solid #363347;
    padding: 5px 0;
  }
  .search-item{
    padding: 5px 0;
    height: 55px;
    display: flex;
    align-items: center;
  }
  .search-item > div{
    flex: 1;
  }
  .v-select.dropdown{
    margin-right: 0;
    border: 1px solid transparent;
  }
  .select-item.empty{
    .v-select{
      border-color: #FF2D55;
    }
  }
  .selected-friend {
    display: flex;
    position: relative;
    img{
      width: auto !important;
      margin-right: 10px;
    }
    .close{
      position: absolute;
      right: 0;
      top: 50%;
      transform: translateY(-50%);
      opacity: 1;
      text-shadow: none;
      text-decoration: none;
      color: #BDCCC9;
    }
  }
  .add-more{
    width: 100%;
    height: 70px;
    display: flex;
    align-items: center;
    justify-content: center;
    &:hover{
      text-decoration: none;
    }
  }
  .add-more img{
    width: 24px !important;
    height: 24px;
    margin-right: 10px;
  }
  .error-message{
    text-align: center;
    color: #FF2D55;
    padding-bottom: 25px;
  }
</style>

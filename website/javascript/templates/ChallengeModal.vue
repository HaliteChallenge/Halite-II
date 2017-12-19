<template>
  <div class="challenger-modal" :class="{'on': isOn}">
    <div class="modal-overlay" @click="close"></div>
    <div class="modal-container">
      <a @click="close" class="close modal-close"><span class="icon-remove"></span></a>
      <div class="send-challenge-form" v-if="!showResult">
        <img class="ico" :src="`${baseUrl}/assets/images/icon-group.svg`">
        <h2 class="heading">CHALLENGE OTHER PLAYERS</h2>
        <p>You can choose one or three players from the leaderboard to challenge. You’ll be able to see how you stack up and we’ll let you know once the challenge is over.</p>
        
        <div class="user-search" v-if="me">
          <div>
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
                  <a :href="'/user?user_id=' + members[friends[index]].user_id"><img width="30" height="30" :src="`https://github.com/${friends[index]}.png`" alt=""> {{friends[index]}}</a>
                  <a class="close" @click="removeFriend(index)"><span class="icon-remove"></span></a>
                </div>
              </div>
            </div>
          </div>
          <a @click="addOpponent" class="add-more">
            <span><img :src="`${baseUrl}/assets/images/icon-add.svg`" alt="">
              Add more opponents</span>
          </a>
        </div>
        <div class="user-search" v-else>
          <p class="no-login">You have to log in first</p>
        </div>
        
        <div class="error-message" v-if="errorMessage">
          {{this.errorMessage}}
        </div>
        <div class="ha-button-container">
          <div>
            <a class="ha-button" @click="submit"><span>SEND CHALLENGE</span></a>
          </div>
        </div>
      </div>
      <div class="send-challenge-result" v-else>
        <img class="ico" :src="`${baseUrl}/assets/images/icon-success.svg`">
        <h2 class="heading">SUCCESS!</h2>
        <p>We’ll let you know when your challenge is complete.</p>
        <div class="ha-button-container">
          <div>
            <a class="ha-button" @click="close"><span>VIEW YOUR RESULTS</span></a>
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
      showResult: false
    }
  },
  mounted: function(){
    api.me().then((me) => {
      if (me){
        this.me = true;
      }
    });

    let options = [];
    let members = {};

    if (this.username){
      this.friends.push(username);
    } else {
      this.friends.push("");
    }

    api.leaderboard(null, null, 0, 9999).then((members) => {
      this.options = members.map((member) => {
        return member.username;
      })
      this.members = members.reduce((result, item) => {
        result[item.username] = item;
        return result;
      })
    });
  },
  watch: {
    username: function(newUsername){
      if (newUsername){
        this.friends[0] = newUsername;
      }
    }
  },
  methods: {
    addOpponent: function(){
      this.friends.push("");
    },
    removeFriend: function(index){
      this.friends[index] = "";
      this.$forceUpdate();
    },
    hasEmptyField: function(index){
      return this.emptyFields.indexOf(index) == -1;
    },
    submit: function(){
      // convert user name to 
      // this.close
      let emptyFields = {};
      let hasError = false;
      this.validated = true; // enable showing error
      this.friends.forEach((item, index) => {
        if (!item){
          emptyFields[index] = 0;
          hasError = true;
        } else {
          emptyFields[index] = 1;
        }
      });

      if (hasError){
        this.errorMessage = "Please select at least one player to challenge";
        this.emptyFields = emptyFields;
      } else {
        this.showResult = true; 
        
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
  }
  .search-item{
    border-top: 1px solid #363347;
    padding: 10px 0;
    height: 70px;
    display: flex;
    align-items: center;
  }
  .search-item:last-child{
    border-bottom: 1px solid #363347;
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
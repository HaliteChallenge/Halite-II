import Message from './templates/Message.vue';
import Vue from 'vue';


export function tierClass(tier){
  const lvl = {"Salt": 5, "Silver": 4, "Gold": 3, "Platinum": 2,"Diamond": 1};
  if (tier in lvl){
    return 'icon-tier-' + lvl[tier];
  } else {
    return '';
  }
}

export const Alert = {
  show: function(message = "there is an error", type = 'error'){
    let outerContainer = document.getElementById('message-placeholder');
    outerContainer.innerHTML = "";
    let container = document.createElement("div");
    outerContainer.appendChild(container);

    new Vue({
      el: container,
      render: (h) => h(Message, {
        props: {
          message: message,
          type: type
        }
      })
    });
  },
  hide: function(){
    let outerContainer = document.getElementById('message-placeholder');
    outerContainer.innerHTML = "";
  }
}
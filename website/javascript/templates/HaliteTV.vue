<template>
  <div class="visuallizer-container">
  <div class="row">
    <div class="col-md-8">
      <div class="game-heading">
        <i class="xline xline-top"></i>
        <i class="xline xline-bottom"></i>
        <p class="game-heading-date">8/3/2017 - 8:08:35 PM</p>
        <div class="game-heading-players">
          <div class="short"></div>
          <div class="long">
            <span class="player color-1"><span class="icon-medal-1"></span>juliak</span>
            <span class="action">defeats</span>
            <span class="player color-2"><span class="icon-medal-2"></span>harikmenon</span>
            <span class="player color-3"><span class="icon-medal-3"></span>CarlosWatton</span>
            <span class="player color-4"><span class="icon-medal-1"></span>erdman</span>
          </div>
        </div>
      </div>
      <div class="game-replay">
        <img class="game-replay-img img-responsive" :src="`${baseUrl}/assets/images/temp/display.png`" alt="">
      </div>
    </div>
    <div class="col-md-4 sidebar">
      <div class="game-stats-widget">
        <ul class="nav nav-tabs" role="tablist">
          <li role="presentation" class="active">
            <a href="#player_stats" aria-controls="player_stats" role="tab" data-toggle="tab">
              <i class="xline xline-top"></i>
              <span>Player stats</span>
              <i class="xline xline-bottom"></i>
            </a>
          </li>
          <li role="presentation">
            <a href="#game_stats" aria-controls="game_stats" role="tab" data-toggle="tab">
              <i class="xline xline-top"></i>
              <span>Game/Map Stats</span>
              <i class="xline xline-bottom"></i>
            </a>
          </li>
        </ul>
        <!-- Tab panes -->
        <div class="tab-content">
          <div role="tabpanel" class="tab-pane active" id="player_stats">
            <table class="player-stats-table">
              <tr class="player-stats">
                <td class="player-stats-name">Juliak:</td>
                <td class="player-stats-break-down">
                  <div class="player-stats-ship">
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                  <div class="player-stats-territory">
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                </td>
              </tr>
              <tr class="player-stats">
                <td class="player-stats-name">Mzotkiew:</td>
                <td class="player-stats-break-down">
                  <div class="player-stats-ship">
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                  <div class="player-stats-territory">
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                </td>
              </tr>
              <tr class="player-stats">
                <td class="player-stats-name">Timfoden:</td>
                <td class="player-stats-break-down">
                  <div class="player-stats-ship">
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                  <div class="player-stats-territory">
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                </td>
              </tr>
              <tr class="player-stats">
                <td class="player-stats-name">Erdman:</td>
                <td class="player-stats-break-down">
                  <div class="player-stats-ship">
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar bar-1"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                  <div class="player-stats-territory">
                    <div class="bar bar-2"></div>
                    <div class="bar bar-2"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                    <div class="bar"></div>
                  </div>
                </td>
              </tr>
              <tr class="play-stats">
                <td></td>
                <td class="player-stats-legend">
                  <p><span class="player-stats-legend-block player-stats-legend-block-1"></span>
                    Relative Ship strength</p>
                  <p><span class="player-stats-legend-block player-stats-legend-block-2"></span>
                    Relative Ship strength</p>
                </td>
              </tr>
            </table>
          </div>
          <div role="tabpanel" class="tab-pane" id="game_stats">...</div>
        </div>
      </div>
      <div class="panel-group" aria-multiselectable="true">
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_player_details">
            <a data-toggle="collapse" data-parent="#accordion" href="#widget_player_details" aria-expanded="false" aria-controls="widget_player_details">
              <i class="xline xline-top"></i>
              <h4>player details</h4>
              <span class="toggle-icon chevron"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse" role="tabpanel" id="widget_player_details" aria-labelledby="heading_player_details">
            <div class="player-cards-list row">
              <div class="col-sm-6">
                <div class="card-player">
                  <h4 class="card-player-name">
                    Juliak V9
                    <span class="circle bg-player-1"></span>
                  </h4>
                  <div class="card-player-stats-list">
                    <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
                    <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
                    <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
                  </div>
                </div>
              </div>
              <div class="col-sm-6">
                <div class="card-player">
                  <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-2"></span></h4>
                  <div class="card-player-stats-list">
                    <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
                    <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
                    <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
                  </div>
                </div>
              </div>
              <div class="col-sm-6">
                <div class="card-player">
                  <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-3"></span></h4>
                  <div class="card-player-stats-list">
                    <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
                    <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
                    <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
                  </div>
                </div>
              </div>
              <div class="col-sm-6">
                <div class="card-player">
                  <h4 class="card-player-name">Juliak V9 <span class="circle bg-player-4"></span></h4>
                  <div class="card-player-stats"><span class="icon-ship"></span> Ships: 5</div>
                  <div class="card-player-stats"><span class="icon-planet"></span> Planet own: 1</div>
                  <div class="card-player-stats"><span class="icon-lightning"></span> Attack: 666</div>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_map_properties">
            <a data-toggle="collapse" data-parent="#accordion" href="#widget_map_properties" aria-expanded="false" aria-controls="widget_map_properties">
              <i class="xline xline-top"></i>
              <h4>map object properties</h4>
              <span class="toggle-icon chevron"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse" role="tabpanel" id="widget_map_properties" aria-labelledby="heading_map_properties">
            <!-- DISPLAY MESSAGE BOX -->
            <!-- <div class="message-box">
              <p><span class="icon-info"></span></p>
              <p>Click on a ship, planet, or other map location to see properties</p>
            </div> -->
            <div class="map-props">
              <div class="map-props-icon">
                <span class="icon-planet"></span>
              </div>
              <div class="map-props-list">
                <table>
                  <tr>
                    <td>Location:</td>
                    <td>181.7349, 166.489</td>
                  </tr>
                  <tr>
                    <td>Selected Planet Owner:</td>
                    <td>Juliak</td>
                  </tr>
                  <tr>
                    <td>Planet ID:</td>
                    <td>11</td>
                  </tr>
                  <tr>
                    <td>Velocity:</td>
                    <td>(0.0000, 0.0000)</td>
                  </tr>
                  <tr>
                    <td>Health:</td>
                    <td>255</td>
                  </tr>
                </table>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
  <div class="post-game-dashboard">
    <div class="panel-group" aria-multiselectable="true">
        <div class="panel panel-stats">
          <div class="panel-heading" role="tab" id="heading_player_details">
            <a data-toggle="collapse" href="#panel_post_game" aria-expanded="false" aria-controls="widget_player_details">
              <i class="xline xline-top"></i>
              <h4>post game dashboard</h4>
              <span class="toggle-icon expand"></span>
              <i class="xline xline-bottom"></i>
            </a>
          </div>
          <div class="panel-collapse collapse" role="tabpanel" id="panel_post_game" aria-labelledby="panel_post_game">
            <div class="card-dashboard-list row">
              <div class="col-md-3">
                <div class="card-dashboard active">
                  <i class="xline xline-top"></i>
                  <i class="xline xline-bottom"></i>
                  <div class="card-dashboard-thumb">
                    <img :src="`${ baseUrl }/assets/images/temp/avatar-1.jpg`">
                  </div>
                  <div class="card-dashboard-info">
                    <span class="dot bg-1"></span>
                    <p class="card-dashboard-name">Juliak</p>
                    <p class="card-dashboard-version-heading">Bot version:</p>
                  </div>
                  <div class="card-dashboard-version">v9</div>
                </div>
              </div>
              <div class="col-md-3">
                <div class="card-dashboard active">
                  <i class="xline xline-top"></i>
                  <i class="xline xline-bottom"></i>
                  <div class="card-dashboard-thumb">
                    <img :src="`${ baseUrl }/assets/images/temp/avatar-1.jpg`">
                  </div>
                  <div class="card-dashboard-info">
                    <span class="dot bg-2"></span>
                    <p class="card-dashboard-name">Juliak</p>
                    <p class="card-dashboard-version-heading">Bot version:</p>
                  </div>
                  <div class="card-dashboard-version">v9</div>
                </div>
              </div>
              <div class="col-md-3">
                <div class="card-dashboard">
                  <i class="xline xline-top"></i>
                  <i class="xline xline-bottom"></i>
                  <div class="card-dashboard-thumb">
                    <img :src="`${ baseUrl }/assets/images/temp/avatar-1.jpg`">
                  </div>
                  <div class="card-dashboard-info">
                    <span class="dot bg-3"></span>
                    <p class="card-dashboard-name">Juliak</p>
                    <p class="card-dashboard-version-heading">Bot version:</p>
                  </div>
                  <div class="card-dashboard-version">v33</div>
                </div>
              </div>
              <div class="col-md-3">
                <div class="card-dashboard active">
                  <i class="xline xline-top"></i>
                  <i class="xline xline-bottom"></i>
                  <div class="card-dashboard-thumb">
                    <img :src="`${ baseUrl }/assets/images/temp/avatar-1.jpg`">
                  </div>
                  <div class="card-dashboard-info">
                    <span class="dot bg-4"></span>
                    <p class="card-dashboard-name">Juliak</p>
                    <p class="card-dashboard-version-heading">Bot version:</p>
                  </div>
                  <div class="card-dashboard-version">v2</div>
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container bt bb">
              <i class="dot-tl"></i>
              <i class="dot-tr"></i>
              <i class="dot-bl"></i>
              <i class="dot-br"></i>
              <div class="dashboard-graph dashboard-graph-full">
                <h4 class="dashboard-graph-heading">
                  <span class="icon-globe"></span>
                  Territory Gained
                </h4>
                <div class="post-game-graph">
                  <!-- TODO: Real Graph -->
                  <img class="post-game-graph-img img-responsive" :src="`${baseUrl}/assets/images/temp/graph-1.png`">
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container bb ">
              <i class="dot-bl"></i>
              <i class="dot-br"></i>
              <div class="row">
                <div class="dashboard-graph br col-md-6">
                  <i class="dot-tr"></i>
                  <i class="dot-br"></i>
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-ship"></span>
                    rate of production
                  </h4>
                  <div class="post-game-graph">
                    <!-- TODO: Real Graph -->
                    <img class="post-game-graph-img img-responsive" :src="`${baseUrl}/assets/images/temp/graph-2.png`">
                  </div>
                </div>
                <div class="dashboard-graph col-md-6">
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-health"></span>
                    health
                  </h4>
                  <div class="post-game-graph">
                    <!-- TODO: Real Graph -->
                    <img class="post-game-graph-img img-responsive" :src="`${baseUrl}/assets/images/temp/graph-3.png`">
                  </div>
                </div>
              </div>
            </div>
            <div class="dashboard-graph-container">
              <div class="row">
                <div class="dashboard-graph br col-md-6">
                  <i class="dot-tr"></i>
                  <i class="dot-br"></i>
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-ship"></span>
                    damage dealed
                  </h4>
                  <div class="post-game-graph">
                    <!-- TODO: Real Graph -->
                    <img class="post-game-graph-img img-responsive" :src="`${baseUrl}/assets/images/temp/graph-4.png`">
                  </div>
                </div>
                <div class="dashboard-graph col-md-6">
                  <h4 class="dashboard-graph-heading">
                    <span class="icon-health"></span>
                    attack overtime
                  </h4>
                  <div class="post-game-graph">
                    <!-- TODO: Real Graph -->
                    <img class="post-game-graph-img img-responsive" :src="`${baseUrl}/assets/images/temp/graph-5.png`">
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
  </div>
</div>
</template>

<script>
    export default {
        name: 'haliteTV',
        props: ['baseUrl']
    }
</script>

<style>
</style>

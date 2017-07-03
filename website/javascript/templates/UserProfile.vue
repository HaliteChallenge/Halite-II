<template>
    <div class="row">
        <!-- TODO: indieweb markup -->
        <div class="col-md-4">
            <img class="img-responsive" src="" :alt="user.username">
            <h1>{{ user.username }}</h1>

            <p>{{ user.level }} at {{ user.organization }}</p>
        </div>
        <div class="col-md-8">
            <section>
                <h2>Stats Summary</h2>

                <p>{{ user.points }} points</p>
                <p>{{ user.num_games }} games played</p>
            </section>
            <section>
                <h2>Recent Games</h2>

                <table class="table">
                </table>
            </section>
        </div>
    </div>
</template>

<script>
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
                },
                games: [],
            };
        },
        mounted: function() {
            const user_id = (new URLSearchParams(window.location.search)).get("user_id");
            $.get("http://35.190.3.178/api/v1/user/" + user_id.toString())
                .then((data) => {
                    this.user = data;
                });
            $.get("http://35.190.3.178/api/v1/user/" + user_id.toString() + "/match")
                .then((data) => {
                    this.games = data;
                    console.log(this);
                });
        },
    }
</script>

<style lang="scss" scoped>

</style>
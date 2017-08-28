<template>
    <ul class="nav navbar-nav navbar-right logged-in">
        <li>
            <a href="/play">Submit a Bot</a>
        </li>
        <li>
            <a href="/user?me" :title="username + '\'s Profile'">
                <img :src="profile_image" class="img-circle" :alt="username + '\'s profile image'" />
            </a>
        </li>
    </ul>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "user-profile-bar",
        data: function() {
            const me = api.me_cached();
            if (me) {
                return {
                    username: me.username,
                    profile_image: api.make_profile_image_url(me.username),
                };
            }
            return {
                username: "",
                profile_image: null,
            };
        },
        mounted: function() {
            api.me().then((user) => {
                this.username = user.username;
                this.profile_image = api.make_profile_image_url(this.username);
            });
        },
    }
</script>

<style lang="scss" scoped>

</style>

<template>
    <div class="col-md-6">
        <halite-upload-zone v-if="logged_in"
                title="Drop BOT here or click to select"
                :message="error"
                v-on:change="upload_bot">
        </halite-upload-zone>
        <div v-else>
            <h2>Log In to upload bot</h2>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";
    import UploadZone from "./UploadZone.vue";

    export default {
        name: "uploader",
        components: {
            "halite-upload-zone": UploadZone,
        },
        data: function() {
            return {
                error: null,
                logged_in: false,
            };
        },
        mounted: function() {
            api.me().then((me) => {
                if (me != null) {
                    this.logged_in = true;
                }
            });
        },
        methods: {
            upload_bot: function(files) {
                if (files.length > 0) {
                    let user_id;
                    const has_bot_promise = api.me().then((user) => {
                        user_id = user.user_id;
                        return api.list_bots(user.user_id);
                    }).then((bots) => {
                        if (bots.length > 0) {
                            return bots[0].bot_id;
                        }
                        return null;
                    });

                    has_bot_promise
                        .then((bot_id) => {
                            return api.update_bot(user_id, bot_id, files[0], (progress) => {
                                this.error = `Uploading (${Math.floor(progress * 100)}%)`;
                            });
                        })
                        .then(() => {
                            this.error = "Successfully uploaded!";
                        }, (error) => {
                            this.error = error.message;
                        });
                }
            }
        },
    }
</script>

<style lang="scss" scoped>
</style>

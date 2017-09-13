<template>
    <div class="upload-bot">
        <halite-upload-zone v-if="logged_in"
            title="Submit a bot"
            description="Drop a .bot file here to upload"
            :icon="`${baseUrl}/assets/images/icon-submit.svg`"
            :message="error"
            :progressBar="is_uploading"
            :progress="progress"
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
                is_uploading: false,
                progress: 0,
                baseUrl: _global.baseUrl
            };
        },
        mounted: function() {
            api.me().then((me) => {
                if (me !== null) {
                    this.logged_in = true;
                }
            });
        },
        methods: {
            upload_bot: function(files) {
                if (files.length > 0) {
                    let user_id;
                    this.error = null;

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
                                this.is_uploading = true;
                                this.progress = Math.floor(progress * 100);
                            });
                        })
                        .then(() => {
                            this.error = "Successfully uploaded!";
                            this.is_uploading = false;
                        }, (error) => {
                            this.error = error.message;
                            this.is_uploading = false;
                        });
                }
            }
        },
    }
</script>

<style lang="scss" scoped>
    .upload-bot {
        // margin-top: 20px;
    }
</style>

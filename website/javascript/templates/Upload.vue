<template>
    <div class="upload-bot">
        <halite-upload-zone v-if="loggedIn"
            title="Submit a bot"
            description="Drop a .zip file here to upload"
            :icon="`${baseUrl}/assets/images/icon-submit.svg`"
            :message="error"
            :progressBar="is_uploading"
            :progress="progress"
            v-on:change="upload_bot">
        </halite-upload-zone>
        <div v-else>
            <div class="panel panel-default upload-zone">
                <div class="panel-body">
                    <h2>Log in to upload bot</h2>
                </div>
            </div>
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
        props: ["loggedIn"],
        data: function() {
            return {
                error: null,
                is_uploading: false,
                progress: 0,
                baseUrl: _global.baseUrl
            };
        },
        methods: {
            upload_bot: function(files) {
                console.log('update files');
                console.log(files);
                if (files.length > 0) {
                    this.$parent.botFile = files[0];
                    this.$parent.currentView = 'botUpload';
                    // let user_id;
                    // this.error = null;

                    // const has_bot_promise = api.me().then((user) => {
                    //     user_id = user.user_id;
                    //     return api.list_bots(user.user_id);
                    // }).then((bots) => {
                    //     if (bots.length > 0) {
                    //         return bots[0].bot_id;
                    //     }
                    //     return null;
                    // });

                    // has_bot_promise
                    //     .then((bot_id) => {
                    //         return api.update_bot(user_id, bot_id, files[0], (progress) => {
                    //             this.is_uploading = true;
                    //             this.progress = Math.floor(progress * 100);
                    //         });
                    //     })
                    //     .then(() => {
                    //         this.error = "Successfully uploaded!";
                    //         this.is_uploading = false;
                    //     }, (error) => {
                    //         this.error = error.message;
                    //         this.is_uploading = false;
                    //     });
                }
            }
        },
    }
</script>

<style lang="scss" scoped>
    .upload-bot {
        // margin-top: 20px;
    }
    h2 {
        text-align: center;
        font-weight: 300;
    }
</style>

<template>
    <form>
        <div class="input-group">
            <label for="user_id">User ID:</label>
            <input id="user_id" type="text" class="form-control" placeholder="User ID" v-model="user_id" />
        </div><!-- /input-group -->
        <div class="input-group">
            <label for="verification_code">Verification Code:</label>
            <input id="verification_code" type="text" class="form-control" placeholder="Verification code" v-model="verification_code" />
        </div><!-- /input-group -->
        <button class="btn btn-submit" type="button" v-on:click="submit" >Verify</button>
        <p class="text-danger">{{ error_message }}</p>
    </form>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "verify-email",
        data: function() {
            return {
                verification_code: "",
                user_id: "",
                error_message: "",
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);

            if (params.has("user_id")) {
                this.user_id = params.get("user_id");
            }
            else {
                api.me().then((me) => {
                    if (me) {
                        this.user_id = me.user_id;
                    }
                });
            }

            if (params.has("verification_code")) {
                this.verification_code = params.get("verification_code");
            }
        },
        methods: {
            submit: function() {
                if (this.verification_code && this.user_id) {
                    $.get({
                        url: `${api.API_SERVER_URL}/user/${this.user_id}/verify`,
                        data: {
                            verification_code: this.verification_code,
                        },
                        error: (xhr) => {
                            this.error_message = xhr.responseJSON.message;
                        },
                    }).then(() => {
                        window.location.replace("/user?me");
                    });
                }
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>

<template>
    <form>
        <p class="text-danger">{{ error_message }}</p>
        <p>{{ success_message }}</p>
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
                success_message: "",
            };
        },
        mounted: function() {
            const params = new URLSearchParams(window.location.search);

            if (params.has("user_id") && params.has("verification_code")) {
                this.user_id = params.get("user_id");
                this.verification_code = params.get("verification_code");
                this.submit();
            }
        },
        methods: {
            submit: function() {
                if (this.verification_code && this.user_id) {
                    $.post({
                        url: `${api.API_SERVER_URL}/user/${this.user_id}/verify`,
                        data: {
                            verification_code: this.verification_code,
                        },
                        error: (xhr) => {
                            this.error_message = "Your email verification failed, please contact halite@halite.io: Error Details -" + xhr.responseJSON.message;
                        },
                        success: (xhr) => {
                            this.success_message = "Your email has been verified successfully. You will be automatically redirected in a few seconds.";
                        },
                    }).then(() => {
                         window.setTimeout(function(){
                            window.location.replace("/play-programming-challenge");
                        }, 3000);
                    });
                }
            }
        },
    }
</script>

<style lang="scss" scoped>

</style>

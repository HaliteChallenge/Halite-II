<template>
    <div class="row">
        <div class="col-md-8">
            <div class="page-header">
                <h1>Reset &amp; Retrieve My API Key</h1>
            </div>
            <p>
                Retrieve your API key for command-line tools, like a command-line bot uploader.
            </p>
            <p>
                <strong>Note:</strong> you may only reset and generate a new API key. Once you close this page, you will not be able to retrieve that key again&mdash;you will have to generate a new one.
            </p>
            <form>
                <button class="btn btn-primary" v-on:click="fetch">
                    Reset and Retrieve API Key
                </button>
                <input type="text" readonly :value="api_key" />
            </form>
        </div>
    </div>
</template>

<script>
    import * as api from "../api";

    export default {
        name: "ApiKey",
        data: function() {
            return {
                api_key: "",
            };
        },
        // TODO: add api method to check login and redirect if appropriate
        methods: {
            fetch: function(e) {
                e.preventDefault();
                api.reset_api_key().then((data) => {
                    console.log(data);
                    this.api_key = data.api_key;
                }, (e) => {
                    console.error(e);
                    this.api_key = "Error: could not reset API key.";
                });
            },
        },
    }
</script>

<style lang="scss" scoped>
    input {
        color: #000;
        width: 100%;
    }
</style>
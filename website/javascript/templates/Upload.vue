<template>
    <div class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-body">
                <h2>Drop or upload BOT files here</h2>
                <form>
                    <input class="form-control" type="file" v-on:change="upload_bot" />
                </form>
            </div>
        </div>
    </div>
</template>

<script>
    export default {
        name: "upload",
        data: function() {
            return {
            };
        },
        mounted: function() {
        },
        methods: {
            upload_bot: function(e) {
                console.log(e);
            },
        },
    }
</script>

<style lang="scss" scoped>

</style>
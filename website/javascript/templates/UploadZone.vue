<template>
    <div class="panel panel-default upload-zone"
         @dragenter="drag_over = true"
         @dragleave="drag_over = false"
         v-bind:class="{ dragging: drag_over }">
        <div class="panel-body">
            <h2>{{ title }}</h2>
            <input class="form-control" type="file" v-on:change="on_changed" />
        </div>
    </div>
</template>

<script>
    export default {
        name: "halite-upload-zone",
        props: ["title"],
        data: function() {
            return {
                drag_over: false,
            };
        },
        methods: {
            on_changed: function(event) {
                this.drag_over = false;
                this.$emit("change", event.target.files);
            },
        },
    }
</script>

<style lang="scss" scoped>
    .upload-zone {
        position: relative;

    h2 {
        text-align: center;
        font-weight: 300;
    }

    input {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100%;
        cursor: pointer;
        opacity: 0;
    }

    &.dragging {
         background: blue;
         color: white;
     }
    }
</style>

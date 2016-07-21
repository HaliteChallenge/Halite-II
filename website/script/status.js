$(function() {
    var workerTable = {
        $tableBody: $("#workerTableBody"),
        init: function(workers) {
            this.workers = workers;
            this.render();
        },
        render: function() {
            this.$tableBody.empty();
            for(var a = 0; a < this.workers.length; a++) {
                this.$tableBody.append(this.getTableRow(this.workers[a]));
            }

        },
        getTableRow: function(worker) {
            console.log(worker.lastRequestTime)
            var timeSinceCommunication = Math.round(100*((new Date() - new Date(worker.lastRequestTime)) / (1000*60))) / 100;
            return "<tr><td>"+worker.name+"</td><td>"+timeSinceCommunication+" min</td></tr>";    
        }
    };

    workerTable.init(getWorkers());
})

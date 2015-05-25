var socket = null;
var cbs    = [];

export default {
    init: function(url) {
        if(socket != null) {
            throw new Error('already initialized');
        }

        socket = new WebSocket(url);
        socket.onmessage = event => {
            const json = JSON.parse(event.data),
                type = json.type,
                data = json.data,
                now = new Date();

            console.log(`${now} [NET] ${type} -> ${JSON.stringify(data)}`);
            cbs
                .filter(cb => cb.type == type)
                .forEach(cb => cb.func(data));
        };
    },

    on: function(type, func) {
        cbs.push({
            type,
            func
        });
    },

    send: function(type, data) {
        socket.send(JSON.stringify({
            type,
            data
        }));
    }
};

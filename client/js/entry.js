import network from './network';

network.init('ws://localhost:8400');
network.send('hello', { name: 'jahan' });

network.on('hello', function(data) {
    console.log(`the server said hello to ${data.name}`);
});

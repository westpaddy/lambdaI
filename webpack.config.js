module.exports = {
    entry: [
        './js/index.jsx'
    ],
    output: {
        path: 'public',
        filename: 'bundle.js'
    },
    resolve: {
        extensions: ['', '.js', '.jsx']
    },
    module: {
        loaders: [
            {
                test: /\.js[x]?$/,
                exclude: /node_modules|calc\.js/,
                loader: "babel",
                query:{
                    presets: ['react', 'es2015']
                }
            }
        ]
    }
};

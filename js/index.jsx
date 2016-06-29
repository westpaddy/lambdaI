var calc = require('./calc.js');

import React from 'react';
import ReactDOM from 'react-dom';
import * as Printer from './printer.jsx';

class Query extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            term: '',
            skel: [undefined],
            infer: [undefined],
        };
    }

    render() {
        if (this.state.infer[0] === "I_Error") {
            return (<div>
                <h4>Inferring a type of</h4>
                <div>{this.state.term}</div>
                <h4>Skeleton</h4>
                <Printer.Deriv val={this.state.skel[2]} />
                <h4>Constraints</h4>
                <Printer.Constraint val={this.state.skel[1]} />
                <h4>{this.state.infer[1]}</h4>
            </div>);
        } else if (this.state.infer[0] === "I_Succ") {
            return (<div>
                <h4>Inferring a type of</h4>
                <div>{this.state.term}</div>
                <h4>Skeleton</h4>
                <Printer.Deriv val={this.state.skel[2]} />
                <h4>Constraints</h4>
                <Printer.Constraint val={this.state.skel[1]} />
                <h4>Unifier</h4>
                <Printer.Subst val={this.state.infer[2]} />
                <h4>Derivation</h4>
                <Printer.Deriv val={this.state.infer[3]} />
                <h4>Inferred Type</h4>
                <Printer.Type val={this.state.infer[1]} />
            </div>);
        } else if (this.state.skel[0] === "S_Error") {
            return (<div>
                <h4>Inferring a type of</h4>
                <div>{this.state.term}</div>
                <h4>{this.state.skel[1]}</h4>
            </div>);
        } else if (this.state.skel[0] === "S_Succ") {
            return (<div>
                <h4>Inferring a type of</h4>
                <div>{this.state.term}</div>
                <h4>Skeleton</h4>
                <Printer.Deriv val={this.state.skel[2]} />
                <h4>Constraints</h4>
                <Printer.Constraint val={this.state.skel[1]} />
                <h4>Inferring...</h4>
            </div>);
        } else {
            return (<div>
                <input type="text" value={this.state.term} onChange={ e => this.handleChange(e) } />
                <button onClick={ e => this.handleClick(e) }>Infer type!</button>
            </div>);
        }
    }

    handleChange(e) {
        const input = e.target;
        this.setState(Object.assign({}, this.state, {
            term: input.value
        }));
    }

    handleClick(e) {
        /* const term = this.state.term + ";;";
         * const skel = JSON.parse(calc.skel(term));
         * this.setState(Object.assign({}, this.state, {
         *     skel: skel
         * }));*/
        this.skeleton().then(e => this.infer(e));
    }

    skeleton() {
        return new Promise((resolve, reject) => {
            const term = this.state.term + ";;";
            const skel = JSON.parse(calc.skel(term));
            this.setState(Object.assign({}, this.state, {
                skel: skel
            }));
            if (skel[0] === "S_Error") {
                reject(skel[1]);
            } else {
                resolve(skel[1]);
            }
        });
    }

    infer() {
        return new Promise((resolve, reject) => {
            const term = this.state.term + ";;";
            const infer = JSON.parse(calc.infer(term));
            this.setState(Object.assign({}, this.state, {
                infer: infer
            }));
            if (infer[0] === "I_Error") {
                reject(infer[1]);
            } else {
                resolve(infer[1]);
            }
        });
    }
}

ReactDOM.render(
    <Query />,
    document.getElementById('content')
);

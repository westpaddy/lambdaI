import React from 'react';

const Term = (props) => {
    const v = props.val;
    switch (v[0]) {
        case "Var":
            return (<span>{v[1]}</span>);
        case "App":
            return (<span>(<Term val={v[1]} /> <Term val={v[2]} />)</span>);
        case "Abs":
            return (<span>(λ{v[1]}.<Term val={v[2]} />)</span>);
        default:
            console.debug("unknown term");
            console.debug(v);
            return (<span></span>);
    };
}

export const Type = (props) => {
    const v = props.val;
    switch (v[0]) {
        case "Var":
            return (<span>α<sub>{v[1][0]}</sub><sup>{v[1][1]}</sup></span>);
        case "Arrow":
            return (<span>(<Type val={v[1]} /> → <Type val={v[2]} />)</span>);
        case "Lift":
            return (<Type val={v[1]} />);
        case "Inter":
            return (<span>(<Type val={v[1]} /> ∧ <Type val={v[2]} />)</span>);
        case "Expand":
            return (<span>(F<sub>{v[1][0]}</sub><sup>{v[1][1]}</sup> <Type val={v[2]} />)</span>);
        default:
            console.debug("unkown type");
            console.debug(v);
            return (<span></span>);
    };
}

class Env extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            visible: false
        };
    }

    render() {
        if (this.state.visible) {
            return (<span className="env" onClick={ e => this.toggle() }>{this.render_assigns()}</span>);
        } else {
            return (<span className="env" onClick={ e => this.toggle() }>E</span>);
        }
    }

    toggle() {
        this.setState(Object.assign({}, this.state, {
            visible: !this.state.visible
        }));
    }

    render_assigns() {
        if (this.props.val.length === 0) {
            return (<span>∅</span>);
        } else if (this.props.val.length === 1) {
            return (<span>{this.props.val[0][0]}:<Type val={this.props.val[0][1]} /></span>);
        } else {
            return (<span>
                <span>{this.props.val[0][0]}:<Type val={this.props.val[0][1]} /></span>
                {this.props.val.slice(1).map(function(assign) {
                     return (<span key={assign.toString()}>; {assign[0]}:<Type val={assign[1]} /></span>);
                 })}
            </span>);
        }
    }
}

export const Constraint = (props) => {
    return (<ul>
        {props.val.map(function(eq) {
        return (<li key={eq.toString()}><Type val={eq[0]} /> ≐ <Type val={eq[1]} /></li>);
        })}
    </ul>);
}

const Expansion = (props) => {
    const v = props.val;
    switch (v[0]) {
        case "E_Hole":
            return (<span>□</span>);
        case "E_Inter":
            return (<span>(<Expansion val={v[1]} /> ∧ <Expansion val={v[2]} />)</span>);
        case "E_Expand":
            return (<span>(F<sub>{v[1][0]}</sub><sup>{v[1][1]}</sup> <Expansion val={v[2]} />)</span>);
        default:
            console.debug("unknown expansion");
            console.debug(v);
            return (<span></span>);
    }
}

export const Subst = (props) => {
    return (<ol>
        {props.val.map(function(st) {
            switch (st[0]) {
                case "T":
                    return (<li key={st.toString()}>α<sub>{st[1][0]}</sub><sup>{st[1][1]}</sup> := <Type val={st[2]} /></li>);
                case "E":
                    return (<li key={st.toString()}>F<sub>{st[1][0]}</sub><sup>{st[1][1]}</sup> := <Expansion val={st[2]} /></li>);
                default:
                    console.debug("unknown subst");
                    console.debug(st);
                    return (<span></span>);
            }
        })}
        </ol>);
}

const Judge = (props) => {
    const env = props.val[0];
    const term = props.val[1];
    const type = props.val[2];
    return (<span><Env val={env} /> ⊢ <Term val={term} /> : <Type val={type} /></span>);
}

export class Deriv extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            visible: true
        };
    }

    render() {
        const v = this.props.val;
        if (this.state.visible) {
            switch (v[0]) {
                case "Var":
                    return (<table><tbody>
            <tr className="premise"><td><div className="pad"></div></td><td rowSpan="2" className="rule"><span onClick={ e => this.toggle() }>Var</span></td></tr>
            <tr className="conclusion"><td><Judge val={v[1]} /></td></tr>
                    </tbody></table>);
                case "Inter":
                case "App":
                    return (<table><tbody>
                        <tr className="premise"><td><Deriv val={v[2]} /></td><td><Deriv val={v[3]} /></td><td rowSpan="2" className="rule"><span onClick={ e => this.toggle() }>{v[0]}</span></td></tr>
                        <tr className="conclusion"><td colSpan="2"><Judge val={v[1]} /></td></tr>
                    </tbody></table>);
                case "Abs_I":
                case "Abs_K":
                case "F":
                    return (<table><tbody>
                        <tr className="premise"><td><Deriv val={v[2]} /></td><td rowSpan="2"  className="rule"><span onClick={ e => this.toggle() }>{v[0]}</span></td></tr>
                        <tr className="conclusion"><td><Judge val={v[1]} /></td></tr>
                    </tbody></table>);
                default:
                    console.debug("unknown derivation");
                    console.debug(v);
                    return (<div></div>);
            }
        } else {
            return (<table><tbody>
                        <tr className="premise"><td>⋮</td><td rowSpan="2" className="rule"><span onClick={ e => this.toggle() }>{v[0]}</span></td></tr>
                        <tr className="conclusion"><td><Judge val={v[1]} /></td></tr>
            </tbody></table>);
        }
    }

    toggle() {
        this.setState(Object.assign({}, this.state, {
            visible: !this.state.visible
        }));
    }
}

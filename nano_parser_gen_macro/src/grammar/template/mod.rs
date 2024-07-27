mod builtins;
mod definition;
mod instance;

/*

%template<bool> OptionB<T> ::= T { $$ = true }
                             | <none> { $$ = false }
                             ;

%template<Option<T>> Option<T> ::= T { $$ = Some($0) }
                                 | <none> { $$ = None }
                                 ;


name: Option
params: [ T ]
type: Option<$0> (replace occurence of params by param ID)
rules: [
    { [ Replace(0) ], "{ $$ = Some($0) }" },
    { [ None ], "{ $$ = None }" },
]
depends: []

%%

Test ::= Option<id> { $$ = $0 };

When parsing Option<id> => {
    if let Some(instance) = find_instance(instances, "Option", ["id"]) {
        symbols.push(instance.nt());
    } else {
        let instance = find_macro(macros, "Option").instanciate(["id"]);
        rules.extend(instance.rules())
        // other things to add
        symbols.push(instance.nt());
        instances.push(instance)
    }
}

*/

pub use builtins::builtin_templates;
pub use definition::{
    Template, TemplateArg, TemplateDepend, TemplateParamType, TemplateRule, TemplateRuleItem,
};
pub use instance::TemplateInstance;

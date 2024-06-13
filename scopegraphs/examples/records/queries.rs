use crate::{PartialType, RecordScopegraph, SgData, SgLabel};
use scopegraphs::resolve::Resolve;
use scopegraphs::{query_regex, Scope};
use scopegraphs_macros::label_order;

pub async fn resolve_record_ref(sg: &RecordScopegraph<'_>, scope: Scope, ref_name: &str) -> Scope {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(SgLabel: Lexical* TypeDefinition))
        .with_data_wellformedness(|record_data: &SgData| match record_data {
            SgData::TypeDecl {
                name: decl_name, ..
            } => decl_name == ref_name,
            _ => false,
        })
        .with_label_order(label_order!(SgLabel: Definition < Lexical))
        .resolve(scope)
        .await;

    *env.get_only_item()
        .expect("record name did not resolve properly")
        .data()
        .expect_type_decl()
}

pub async fn resolve_lexical_ref(
    sg: &RecordScopegraph<'_>,
    scope: Scope,
    var_name: &str,
) -> PartialType {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(SgLabel: Lexical* Definition))
        .with_label_order(label_order!(SgLabel:  Definition < Lexical))
        .with_data_wellformedness(|record_data: &SgData| -> bool {
            matches!(record_data, SgData::VarDecl { name, .. } if name == var_name)
        })
        .resolve(scope)
        .await;

    env.get_only_item()
        .expect("variable did not resolve uniquely")
        .data()
        .expect_var_decl()
        .clone()
}

pub async fn resolve_member_ref(
    sg: &RecordScopegraph<'_>,
    record_scope: Scope,
    ref_name: &str,
) -> PartialType {
    let env = sg
        .query()
        .with_path_wellformedness(query_regex!(SgLabel: Definition))
        .with_data_wellformedness(|record_data: &SgData| match record_data {
            SgData::VarDecl {
                name: decl_name, ..
            } => decl_name == ref_name,
            _ => false,
        })
        .resolve(record_scope)
        .await;

    env.get_only_item()
        .expect("field name did not resolve properly")
        .data()
        .expect_var_decl()
        .clone()
}

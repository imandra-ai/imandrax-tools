(value_definition
    (let_binding
        pattern: (value_name) @function_name
        (item_attribute
            "[@@"
            (attribute_id) @_measure_id
            (attribute_payload) @measure_payload
            (#eq? @_measure_id "measure")
        ) @measure_attr
    )
) @function_definition

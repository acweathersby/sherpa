use lsp_types::{
    request::Completion, CompletionItem, CompletionItemKind, CompletionOptions, CompletionResponse,
    Documentation, ServerCapabilities,
};
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        completion_provider: Some(CompletionOptions {
            ..Default::default()
        }),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    eprintln!("Sever Initialized");
    // ... Run main loop ...
    for msg in &connection.receiver {
        match msg {
            Message::Notification(notification) => {
                eprintln!("Got notification {:?}", notification);
            }
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                match cast::<Completion>(req) {
                    Ok((id, params)) => {
                        eprintln!("got Completion request #{}: {:?}", id, params);
                        let result = Some(CompletionResponse::Array(vec![CompletionItem {
                            label: String::from("test"),
                            kind: Some(CompletionItemKind::CLASS),
                            additional_text_edits: None,
                            command: None,
                            commit_characters: None,
                            data: None,
                            deprecated: None,
                            detail: None,
                            documentation: Some(Documentation::String("A test entry".to_string())),
                            filter_text: None,
                            insert_text: Some(String::from("test")),
                            insert_text_format: None,
                            insert_text_mode: None,
                            preselect: Some(true),
                            sort_text: None,
                            tags: None,
                            text_edit: None,
                        }]));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            }
            Message::Response(res) => {
                eprintln!("Got response notification {:?}", res);
            }
        }
    }

    io_threads.join()?;
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

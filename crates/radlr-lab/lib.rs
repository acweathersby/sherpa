//!
//! # RADLR LAB

use radlr_core::{ParserConfig, ParserStore, RadlrDatabase, RadlrError, RadlrGrammar, RadlrIRParser, RadlrResult};
use std::path::PathBuf;

pub mod serialize;

#[repr(u8)]
enum WSRequestCodes {
  Undefined    = 0,
  /**
   * Request to build grammar artifacts (parser, states, ast, etc)
   */
  BuildGrammar = 1,
}

#[repr(u8)]
enum WSResponseCodes {
  Undefined = 0,
}

#[cfg(feature = "host")]
pub fn run_lab_server(port: u16) -> Result<(), RadlrError> {
  let port = "15421";
  let ip4 = "0.0.0.0";

  use std::{net::TcpListener, thread::spawn};
  use tungstenite::*;

  let server = TcpListener::bind(&format!("{ip4}:{port}"))?;

  println!("Listening on {ip4}:{port}");

  let mut sockets: Vec<std::thread::JoinHandle<Result<(), RadlrError>>> = Vec::new();

  for stream in server.incoming() {
    for socket_handle in sockets.drain(..).collect::<Vec<_>>() {
      if socket_handle.is_finished() {
        if let Err(err) = socket_handle.join() {
          eprintln!("{err:?}");
        }
      } else {
        sockets.push(socket_handle);
      }
    }

    match stream {
      Ok(stream) => {
        println!("Connected to {:?}", stream.peer_addr().expect("Could not read peer address. Somehow, this is fatal."));

        let socket_handle = spawn(move || -> Result<(), RadlrError> {
          match accept(stream) {
            Ok(mut websocket) => loop {
              match websocket.read() {
                Ok(msg) => match msg {
                  Message::Ping(ping) => {
                    println!("Ping received {ping:?}");
                  }
                  Message::Pong(pong) => {
                    println!("Pong received {pong:?}");
                  }
                  Message::Text(text_data) => {
                    println!("Text received {text_data:?}");
                  }
                  Message::Binary(binary_data) => {
                    if binary_data.len() == 0 {
                      println!("Ignoring empty binary");
                      continue;
                    }
                    match unsafe { std::mem::transmute::<_, WSRequestCodes>(binary_data[0]) } {
                      WSRequestCodes::BuildGrammar => {
                        println!("Request to build grammar");

                        const CONFIG_SIZE: usize = size_of::<ParserConfig>();

                        if binary_data.len() < 8 {
                          eprintln!("Invalid minimum length {}; expected at least 8 bytes transmitted.", binary_data.len());
                        }

                        // SAFETY: assumes the target machine is little endian
                        let mut string_len: u32 = 0;

                        unsafe { binary_data.as_ptr().offset(4).copy_to((&mut string_len) as *mut _ as *mut u8, 4) };

                        println!("ParserConfig::size {}", CONFIG_SIZE);

                        let parser_config: ParserConfig =
                          unsafe { std::mem::transmute_copy(&*(binary_data.as_ptr().offset(8) as *const ParserConfig)) };

                        let string_off = CONFIG_SIZE + 8;

                        if (binary_data.len() - string_off) < string_len as usize {
                          eprintln!("Invalid string length {}; expected string length of {}", binary_data.len() - 8, string_len);
                          continue;
                        } else if string_len == 0 {
                          eprintln!("String section is empty");
                          continue;
                        }

                        println!("{parser_config:#?}");

                        match String::from_utf8(binary_data[string_off..string_off + string_len as usize].to_vec()) {
                          Ok(grammar_string) => {
                            println!("Grammar is:\n======\n{grammar_string}\n=====");

                            println!("Compiling Grammar!");

                            let grammar_path = PathBuf::from("/grammar.radlr".to_string());

                            match RadlrGrammar::new()
                              .add_source_from_string(&grammar_string, &grammar_path, false)?
                              .build_db(&grammar_path, parser_config)
                            {
                              Ok(db) => {
                                fn build_states(
                                  db: &RadlrDatabase,
                                  parser_config: ParserConfig,
                                  optimize: bool,
                                ) -> RadlrResult<RadlrIRParser> {
                                  let pool = radlr_core::worker_pool::StandardPool::new(20).unwrap();
                                  let states = db.build_states(parser_config, &pool)?;
                                  let parser = states.build_ir_parser(optimize, false, &pool)?;

                                  println!("Parser Result \n{}", parser.report.to_string());
                                  Ok(parser)
                                }

                                let parser = match build_states(&db, parser_config, true) {
                                  Ok(parser) => parser,
                                  Err(err) => {
                                    eprintln!("{err}");
                                    continue;
                                  }
                                };

                                println!("Built parser: Type {:?}", parser.get_classification().to_string());

                                //Ok(parser)
                              }
                              Err(err) => {
                                eprintln!("Errors encountered in grammar {err}");
                                continue;
                              }
                            }
                          }
                          Err(err) => {
                            eprintln!("Failed to decode utf8 data:\n{err}");
                          }
                        }
                      }
                      _ => {}
                    }
                  }
                  Message::Close(_) => {
                    println!("Close received");
                    break Ok(());
                  }
                  _ => {}
                },
                Err(err) => break Err(RadlrError::IOError(format!("{err:?}"))),
              }
            },
            Err(err) => Err(RadlrError::IOError(err.to_string())),
          }
        });
        sockets.push(socket_handle);
      }
      Err(err) => {
        eprintln!("{err} \n Exiting due to error");
        break;
      }
    }
  }

  Ok(())
}

// - Get Parser States
// - Get

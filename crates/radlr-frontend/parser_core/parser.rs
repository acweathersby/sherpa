#![allow(unused)]

/// ### `radlr` Rust Parser
///
/// - **GENERATOR**: radlr 1.0.1-beta2
/// - **SOURCE**: UNDEFINED
///
/// #### WARNING WARNING WARNING WARNING
/// #### WARNING WARNING WARNING WARNING
/// #### WARNING WARNING WARNING WARNING
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
/// #### GNINRAW GNINRAW GNINRAW GNINRAW
///
/// #### License:

/// Copyright (c) 2020-2024 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the 'Software'), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE

use radlr_rust_runtime::{kernel::ByteCodeParserNew, parsers::Parser, types::*, *};
use std::{collections::HashMap, rc::Rc};

const BINARY: &'static [u8] = include_bytes!("./parser.bin");

const NONTERM_NAME_TO_ID: [(&'static str, u32); 3] = [("ast",1),("grammar",2),("ir",0),];

const TOKEN_ID_TO_STRING: [(u32, &'static str); 141] = [
  (0, r###"Default"###),
  (1, r###"c:sp"###),
  (10, r###"<nonterm_declarations>"###),
  (100, r###"f32"###),
  (101, r###"i32"###),
  (102, r###"u32"###),
  (103, r###"f64"###),
  (104, r###"i64"###),
  (105, r###"u64"###),
  (106, r###"f16"###),
  (107, r###"i16"###),
  (108, r###"u16"###),
  (109, r###"f128"###),
  (11, r###"("###),
  (110, r###"i128"###),
  (111, r###"u128"###),
  (112, r###"true"###),
  (113, r###"false"###),
  (114, r###"tk:"###),
  (115, r###"tk:( "\\" c:id+)"###),
  (116, r###"\n"###),
  (117, r###"\s"###),
  (118, r###"\id"###),
  (119, r###"\tab"###),
  (12, r###")"###),
  (120, r###"\sym"###),
  (121, r###"\any"###),
  (122, r###"\vtab"###),
  (123, r###"(*"###),
  (124, r###"(+"###),
  (125, r###"<token>"###),
  (126, r###"tk:precedence_num"###),
  (127, r###"kw"###),
  (128, r###"default"###),
  (129, r###"fail-hint"###),
  (13, r###"then"###),
  (130, r###"<string>"###),
  (131, r###"<terminal>"###),
  (
    132, r###"tk:( ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | '-' )? c:num(+) )? )"###
  ),
  (133, r###"tk:( "\"" ( c:id | c:num | c:nl | c:sym | c:sp | escaped )(*) "\"" )"###),
  (134, r###"tk:( "'" ( c:id | c:num | c:nl | c:sym | c:sp | escaped )(*) "'" )"###),
  (135, r###"tk:( c:num(+) )"###),
  (136, r###"[^"###),
  (137, r###""###),
  (138, r###"\"###),
  (139, r###"\d"###),
  (14, r###"<pop>"###),
  (140, r###"\w"###),
  (15, r###"<transitive_statement>"###),
  (16, r###"<branch_statement>"###),
  (17, r###","###),
  (18, r###"<type_identifier>"###),
  (19, r###"::"###),
  (2, r###"c:nl"###),
  (20, r###"|"###),
  (21, r###"to"###),
  (22, r###"rule"###),
  (23, r###"with"###),
  (24, r###":ast"###),
  (25, r###"reduce"###),
  (26, r###"set-tok"###),
  (27, r###"symbols"###),
  (28, r###"set-line"###),
  (29, r###"set-tok-len"###),
  (3, r###"=>"###),
  (30, r###"<int>"###),
  (31, r###"<nonterm_symbol>"###),
  (32, r###"<body>"###),
  (33, r###"pop"###),
  (34, r###"tok"###),
  (35, r###"peek"###),
  (36, r###"char"###),
  (37, r###"-skip"###),
  (38, r###"reset"###),
  (39, r###"shift"###),
  (4, r###"=!>"###),
  (40, r###":"###),
  (41, r###"tk:( 't' "_"{:9999} )"###),
  (42, r###"tk:( 'f' "_"{:9999} )"###),
  (43, r###"%"###),
  (44, r###"*"###),
  (45, r###"+"###),
  (46, r###"-"###),
  (47, r###"/"###),
  (48, r###"^"###),
  (49, r###"<expr>"###),
  (5, r###"<nonterm>"###),
  (50, r###"<term>"###),
  (51, r###"AS"###),
  (52, r###"as"###),
  (53, r###"EXPORT"###),
  (54, r###"c:id"###),
  (55, r###"c:num"###),
  (56, r###"c:sym"###),
  (57, r###"IMPORT"###),
  (58, r###"NAME"###),
  (59, r###"IGNORE"###),
  (6, r###"<line>"###),
  (60, r###"<ignore_clause>"###),
  (61, r###"<standard_nonterm_declarations>"###),
  (62, r###">"###),
  (63, r###"<>"###),
  (64, r###"<rules>"###),
  (65, r###"tk:( ( "-" | "_" | c:id ) ( c:id | '_' | '-' | c:num )(*) )"###),
  (66, r###"<end_of_input>"###),
  (67, r###"tk:( c:num(+) )"###),
  (68, r###"fail"###),
  (69, r###"pass"###),
  (7, r###"<block>"###),
  (70, r###"accept"###),
  (71, r###"tk"###),
  (72, r###"token"###),
  (73, r###"<range>"###),
  (74, r###"tk:("###),
  (75, r###"?"###),
  (76, r###"<precedence>"###),
  (77, r###"<repetition>"###),
  (78, r###"<reference>"###),
  (79, r###"!"###),
  (8, r###"{"###),
  (80, r###"["###),
  (81, r###"]"###),
  (82, r###"]!?"###),
  (83, r###"<not_empty_set>"###),
  (84, r###"$"###),
  (85, r###"match"###),
  (86, r###"<id>"###),
  (87, r###"PRODUCTION"###),
  (88, r###"TERMINAL"###),
  (89, r###"push"###),
  (9, r###"}"###),
  (90, r###"goto"###),
  (91, r###"fork"###),
  (92, r###"<"###),
  (93, r###"."###),
  (94, r###"map"###),
  (95, r###"str"###),
  (96, r###"<convert_initializer>"###),
  (97, r###"bool"###),
  (98, r###"i8"###),
  (99, r###"u8"###),
];

const NONTERM_ID_TO_ADDRESS: [(u32, u32); 3] = [(0, 8),(1, 269466),(2, 270675),];

const STATE_TO_TOKEN_IDS: [(u32, &'static [u32]); 771] = [
  (100150, &TOKENS_5),
  (10041, &TOKENS_59),
  (10098, &TOKENS_102),
  (10110, &TOKENS_118),
  (101275, &TOKENS_7),
  (101411, &TOKENS_5),
  (10197, &TOKENS_82),
  (10253, &TOKENS_16),
  (102536, &TOKENS_7),
  (102642, &TOKENS_5),
  (10316, &TOKENS_30),
  (103472, &TOKENS_5),
  (10397, &TOKENS_72),
  (104597, &TOKENS_7),
  (10465, &TOKENS_16),
  (104703, &TOKENS_5),
  (10570, &TOKENS_16),
  (105828, &TOKENS_7),
  (105958, &TOKENS_5),
  (10634, &TOKENS_16),
  (106788, &TOKENS_5),
  (10697, &TOKENS_68),
  (10760, &TOKENS_81),
  (107913, &TOKENS_7),
  (108043, &TOKENS_5),
  (10816, &TOKENS_108),
  (10872, &TOKENS_38),
  (108873, &TOKENS_5),
  (10941, &TOKENS_59),
  (10998, &TOKENS_102),
  (109998, &TOKENS_7),
  (11010, &TOKENS_81),
  (110104, &TOKENS_5),
  (11066, &TOKENS_99),
  (110934, &TOKENS_5),
  (112059, &TOKENS_7),
  (112129, &TOKENS_5),
  (112959, &TOKENS_5),
  (113789, &TOKENS_5),
  (114, &TOKENS_56),
  (114935, &TOKENS_6),
  (115065, &TOKENS_5),
  (116190, &TOKENS_6),
  (116296, &TOKENS_5),
  (117126, &TOKENS_5),
  (118251, &TOKENS_6),
  (118357, &TOKENS_5),
  (1191, &TOKENS_2),
  (11912, &TOKENS_99),
  (119187, &TOKENS_5),
  (11924, &TOKENS_70),
  (1203, &TOKENS_43),
  (120312, &TOKENS_6),
  (12033, &TOKENS_52),
  (120442, &TOKENS_5),
  (121272, &TOKENS_5),
  (12154, &TOKENS_23),
  (12212, &TOKENS_39),
  (122397, &TOKENS_6),
  (122467, &TOKENS_5),
  (12263, &TOKENS_48),
  (123297, &TOKENS_5),
  (12412, &TOKENS_39),
  (124422, &TOKENS_6),
  (124552, &TOKENS_5),
  (12463, &TOKENS_19),
  (125382, &TOKENS_5),
  (12600, &TOKENS_17),
  (126507, &TOKENS_6),
  (126613, &TOKENS_5),
  (12663, &TOKENS_17),
  (12726, &TOKENS_19),
  (127443, &TOKENS_34),
  (127455, &TOKENS_34),
  (127467, &TOKENS_16),
  (127913, &TOKENS_21),
  (128032, &TOKENS_5),
  (12863, &TOKENS_17),
  (128862, &TOKENS_5),
  (12926, &TOKENS_19),
  (129692, &TOKENS_5),
  (13063, &TOKENS_17),
  (130817, &TOKENS_49),
  (131160, &TOKENS_21),
  (131256, &TOKENS_5),
  (13126, &TOKENS_99),
  (13138, &TOKENS_8),
  (13150, &TOKENS_83),
  (132086, &TOKENS_5),
  (13231, &TOKENS_42),
  (13243, &TOKENS_42),
  (13255, &TOKENS_70),
  (13267, &TOKENS_42),
  (133211, &TOKENS_21),
  (133331, &TOKENS_5),
  (13376, &TOKENS_19),
  (134161, &TOKENS_5),
  (13513, &TOKENS_52),
  (135286, &TOKENS_21),
  (135346, &TOKENS_5),
  (136176, &TOKENS_5),
  (13634, &TOKENS_23),
  (13692, &TOKENS_39),
  (137301, &TOKENS_21),
  (137397, &TOKENS_5),
  (13743, &TOKENS_48),
  (138227, &TOKENS_5),
  (13892, &TOKENS_39),
  (139352, &TOKENS_21),
  (13943, &TOKENS_19),
  (139448, &TOKENS_5),
  (140278, &TOKENS_5),
  (14080, &TOKENS_19),
  (141403, &TOKENS_21),
  (141523, &TOKENS_5),
  (14217, &TOKENS_17),
  (142353, &TOKENS_5),
  (143183, &TOKENS_5),
  (144013, &TOKENS_5),
  (144843, &TOKENS_5),
  (145968, &TOKENS_7),
  (146104, &TOKENS_5),
  (146934, &TOKENS_5),
  (14719, &TOKENS_33),
  (14770, &TOKENS_56),
  (147764, &TOKENS_5),
  (148594, &TOKENS_5),
  (149719, &TOKENS_7),
  (149825, &TOKENS_5),
  (150655, &TOKENS_5),
  (151780, &TOKENS_7),
  (151886, &TOKENS_5),
  (152716, &TOKENS_5),
  (153841, &TOKENS_7),
  (153971, &TOKENS_5),
  (154801, &TOKENS_5),
  (155926, &TOKENS_7),
  (156056, &TOKENS_5),
  (156886, &TOKENS_5),
  (158011, &TOKENS_7),
  (158117, &TOKENS_5),
  (158947, &TOKENS_5),
  (15931, &TOKENS_100),
  (15982, &TOKENS_22),
  (160072, &TOKENS_7),
  (160142, &TOKENS_5),
  (16033, &TOKENS_2),
  (16072, &TOKENS_8),
  (160972, &TOKENS_22),
  (161039, &TOKENS_11),
  (161051, &TOKENS_90),
  (161091, &TOKENS_22),
  (16111, &TOKENS_33),
  (161158, &TOKENS_5),
  (16123, &TOKENS_33),
  (161988, &TOKENS_5),
  (162818, &TOKENS_93),
  (163665, &TOKENS_5),
  (164495, &TOKENS_5),
  (16453, &TOKENS_2),
  (16492, &TOKENS_2),
  (16531, &TOKENS_2),
  (165325, &TOKENS_5),
  (16570, &TOKENS_8),
  (166155, &TOKENS_5),
  (167280, &TOKENS_7),
  (167416, &TOKENS_5),
  (168246, &TOKENS_5),
  (169076, &TOKENS_5),
  (169906, &TOKENS_5),
  (171031, &TOKENS_7),
  (171137, &TOKENS_5),
  (171967, &TOKENS_5),
  (173092, &TOKENS_7),
  (173198, &TOKENS_5),
  (174028, &TOKENS_5),
  (17462, &TOKENS_8),
  (17501, &TOKENS_8),
  (175153, &TOKENS_7),
  (175283, &TOKENS_5),
  (17540, &TOKENS_2),
  (17579, &TOKENS_2),
  (176113, &TOKENS_5),
  (17618, &TOKENS_2),
  (17657, &TOKENS_2),
  (17696, &TOKENS_47),
  (177238, &TOKENS_7),
  (177368, &TOKENS_5),
  (17777, &TOKENS_103),
  (178198, &TOKENS_5),
  (17840, &TOKENS_35),
  (17891, &TOKENS_8),
  (17930, &TOKENS_84),
  (179323, &TOKENS_7),
  (179429, &TOKENS_5),
  (17981, &TOKENS_8),
  (18020, &TOKENS_46),
  (180259, &TOKENS_5),
  (18089, &TOKENS_57),
  (181384, &TOKENS_7),
  (181454, &TOKENS_5),
  (18146, &TOKENS_8),
  (18185, &TOKENS_84),
  (182284, &TOKENS_5),
  (18236, &TOKENS_8),
  (18275, &TOKENS_58),
  (183114, &TOKENS_5),
  (18344, &TOKENS_35),
  (18395, &TOKENS_8),
  (184256, &TOKENS_14),
  (18434, &TOKENS_57),
  (184385, &TOKENS_5),
  (18491, &TOKENS_8),
  (185215, &TOKENS_5),
  (18530, &TOKENS_2),
  (18569, &TOKENS_2),
  (186045, &TOKENS_5),
  (18608, &TOKENS_2),
  (18647, &TOKENS_2),
  (186875, &TOKENS_5),
  (18746, &TOKENS_47),
  (18785, &TOKENS_52),
  (188000, &TOKENS_14),
  (188106, &TOKENS_5),
  (188936, &TOKENS_5),
  (18901, &TOKENS_23),
  (18959, &TOKENS_39),
  (190061, &TOKENS_14),
  (19010, &TOKENS_48),
  (190167, &TOKENS_5),
  (190997, &TOKENS_5),
  (19154, &TOKENS_47),
  (19193, &TOKENS_39),
  (192122, &TOKENS_14),
  (1922, &TOKENS_43),
  (192228, &TOKENS_5),
  (19244, &TOKENS_19),
  (193058, &TOKENS_5),
  (19376, &TOKENS_47),
  (19415, &TOKENS_17),
  (194183, &TOKENS_14),
  (194313, &TOKENS_5),
  (19473, &TOKENS_47),
  (19512, &TOKENS_17),
  (195143, &TOKENS_5),
  (19570, &TOKENS_47),
  (19609, &TOKENS_47),
  (196268, &TOKENS_14),
  (196398, &TOKENS_5),
  (19648, &TOKENS_19),
  (197228, &TOKENS_5),
  (19780, &TOKENS_47),
  (19819, &TOKENS_17),
  (198353, &TOKENS_14),
  (198423, &TOKENS_5),
  (19877, &TOKENS_47),
  (19916, &TOKENS_47),
  (199253, &TOKENS_115),
  (199329, &TOKENS_5),
  (19955, &TOKENS_19),
  (200459, &TOKENS_14),
  (200589, &TOKENS_49),
  (200601, &TOKENS_94),
  (200685, &TOKENS_86),
  (200762, &TOKENS_23),
  (200820, &TOKENS_5),
  (20087, &TOKENS_47),
  (20126, &TOKENS_17),
  (201650, &TOKENS_5),
  (20184, &TOKENS_47),
  (20223, &TOKENS_8),
  (202480, &TOKENS_5),
  (20262, &TOKENS_8),
  (203310, &TOKENS_5),
  (204140, &TOKENS_5),
  (205265, &TOKENS_7),
  (205401, &TOKENS_5),
  (206231, &TOKENS_5),
  (207061, &TOKENS_5),
  (207891, &TOKENS_5),
  (209016, &TOKENS_7),
  (209122, &TOKENS_5),
  (209952, &TOKENS_5),
  (21, &TOKENS_83),
  (211077, &TOKENS_7),
  (211183, &TOKENS_5),
  (21154, &TOKENS_8),
  (21166, &TOKENS_8),
  (21178, &TOKENS_47),
  (212013, &TOKENS_5),
  (21259, &TOKENS_103),
  (213138, &TOKENS_7),
  (21322, &TOKENS_35),
  (213268, &TOKENS_5),
  (21378, &TOKENS_84),
  (214098, &TOKENS_5),
  (21434, &TOKENS_58),
  (21503, &TOKENS_57),
  (215223, &TOKENS_7),
  (215353, &TOKENS_5),
  (21560, &TOKENS_35),
  (21616, &TOKENS_46),
  (216183, &TOKENS_5),
  (21685, &TOKENS_57),
  (217308, &TOKENS_7),
  (217414, &TOKENS_5),
  (21742, &TOKENS_84),
  (21798, &TOKENS_2),
  (218244, &TOKENS_5),
  (21907, &TOKENS_52),
  (219369, &TOKENS_7),
  (219439, &TOKENS_5),
  (220269, &TOKENS_5),
  (22028, &TOKENS_23),
  (22086, &TOKENS_39),
  (221099, &TOKENS_5),
  (22137, &TOKENS_48),
  (222224, &TOKENS_44),
  (222354, &TOKENS_5),
  (22286, &TOKENS_39),
  (22337, &TOKENS_19),
  (223479, &TOKENS_7),
  (223616, &TOKENS_5),
  (224446, &TOKENS_5),
  (22474, &TOKENS_19),
  (225276, &TOKENS_5),
  (226106, &TOKENS_5),
  (22611, &TOKENS_19),
  (227231, &TOKENS_44),
  (227337, &TOKENS_5),
  (22748, &TOKENS_8),
  (22760, &TOKENS_122),
  (228167, &TOKENS_5),
  (22841, &TOKENS_15),
  (22910, &TOKENS_113),
  (229292, &TOKENS_44),
  (229398, &TOKENS_5),
  (22967, &TOKENS_112),
  (230228, &TOKENS_5),
  (23023, &TOKENS_55),
  (23086, &TOKENS_78),
  (231353, &TOKENS_44),
  (23142, &TOKENS_112),
  (231483, &TOKENS_5),
  (23198, &TOKENS_87),
  (232313, &TOKENS_5),
  (23267, &TOKENS_78),
  (23323, &TOKENS_113),
  (233438, &TOKENS_44),
  (233568, &TOKENS_5),
  (23380, &TOKENS_106),
  (234398, &TOKENS_5),
  (23451, &TOKENS_101),
  (23511, &TOKENS_64),
  (235523, &TOKENS_44),
  (235629, &TOKENS_5),
  (236459, &TOKENS_5),
  (237584, &TOKENS_44),
  (237654, &TOKENS_5),
  (238484, &TOKENS_14),
  (238496, &TOKENS_10),
  (238561, &TOKENS_73),
  (238855, &TOKENS_17),
  (238867, &TOKENS_17),
  (238942, &TOKENS_116),
  (239019, &TOKENS_17),
  (239082, &TOKENS_28),
  (239094, &TOKENS_7),
  (239106, &TOKENS_10),
  (239171, &TOKENS_28),
  (239183, &TOKENS_7),
  (239195, &TOKENS_10),
  (239260, &TOKENS_28),
  (239272, &TOKENS_10),
  (239337, &TOKENS_28),
  (239349, &TOKENS_10),
  (239414, &TOKENS_28),
  (239426, &TOKENS_10),
  (239491, &TOKENS_28),
  (239503, &TOKENS_10),
  (239568, &TOKENS_28),
  (239580, &TOKENS_10),
  (239645, &TOKENS_28),
  (239657, &TOKENS_10),
  (239722, &TOKENS_28),
  (239734, &TOKENS_10),
  (239799, &TOKENS_28),
  (239811, &TOKENS_10),
  (239876, &TOKENS_28),
  (239888, &TOKENS_10),
  (239953, &TOKENS_28),
  (239965, &TOKENS_10),
  (240030, &TOKENS_28),
  (240042, &TOKENS_10),
  (240107, &TOKENS_28),
  (240119, &TOKENS_10),
  (240184, &TOKENS_28),
  (240196, &TOKENS_10),
  (240261, &TOKENS_28),
  (240273, &TOKENS_6),
  (240285, &TOKENS_7),
  (240597, &TOKENS_49),
  (24068, &TOKENS_39),
  (240716, &TOKENS_5),
  (24127, &TOKENS_124),
  (241546, &TOKENS_5),
  (24232, &TOKENS_120),
  (242376, &TOKENS_5),
  (24280, &TOKENS_94),
  (243206, &TOKENS_5),
  (24373, &TOKENS_26),
  (24432, &TOKENS_60),
  (244331, &TOKENS_49),
  (244427, &TOKENS_5),
  (245257, &TOKENS_5),
  (246382, &TOKENS_49),
  (246478, &TOKENS_5),
  (247308, &TOKENS_5),
  (248433, &TOKENS_49),
  (248553, &TOKENS_5),
  (249383, &TOKENS_5),
  (250508, &TOKENS_49),
  (250568, &TOKENS_5),
  (251398, &TOKENS_5),
  (252523, &TOKENS_49),
  (252643, &TOKENS_5),
  (253473, &TOKENS_5),
  (254598, &TOKENS_49),
  (254694, &TOKENS_5),
  (255524, &TOKENS_11),
  (255536, &TOKENS_11),
  (255548, &TOKENS_117),
  (255624, &TOKENS_77),
  (255672, &TOKENS_39),
  (255736, &TOKENS_26),
  (255795, &TOKENS_60),
  (25624, &TOKENS_88),
  (2569, &TOKENS_17),
  (256987, &TOKENS_11),
  (256999, &TOKENS_117),
  (257075, &TOKENS_77),
  (257123, &TOKENS_39),
  (257187, &TOKENS_39),
  (257251, &TOKENS_39),
  (257644, &TOKENS_11),
  (257703, &TOKENS_37),
  (258077, &TOKENS_11),
  (258124, &TOKENS_11),
  (258183, &TOKENS_40),
  (258462, &TOKENS_16),
  (258474, &TOKENS_16),
  (258486, &TOKENS_65),
  (258534, &TOKENS_27),
  (258606, &TOKENS_65),
  (258618, &TOKENS_16),
  (258630, &TOKENS_11),
  (258677, &TOKENS_39),
  (258741, &TOKENS_11),
  (258800, &TOKENS_40),
  (259079, &TOKENS_16),
  (259091, &TOKENS_114),
  (259173, &TOKENS_20),
  (259243, &TOKENS_20),
  (259313, &TOKENS_114),
  (259395, &TOKENS_20),
  (259465, &TOKENS_11),
  (259524, &TOKENS_37),
  (260228, &TOKENS_11),
  (260288, &TOKENS_37),
  (260665, &TOKENS_11),
  (260712, &TOKENS_11),
  (260772, &TOKENS_40),
  (261054, &TOKENS_16),
  (261067, &TOKENS_16),
  (261080, &TOKENS_11),
  (261140, &TOKENS_40),
  (261422, &TOKENS_16),
  (261435, &TOKENS_11),
  (261482, &TOKENS_11),
  (261541, &TOKENS_40),
  (261820, &TOKENS_16),
  (261832, &TOKENS_16),
  (261844, &TOKENS_50),
  (261892, &TOKENS_121),
  (261975, &TOKENS_12),
  (262071, &TOKENS_75),
  (262203, &TOKENS_64),
  (262745, &TOKENS_16),
  (262798, &TOKENS_13),
  (262810, &TOKENS_29),
  (262858, &TOKENS_64),
  (263400, &TOKENS_16),
  (263453, &TOKENS_13),
  (263465, &TOKENS_29),
  (263513, &TOKENS_62),
  (263572, &TOKENS_16),
  (263732, &TOKENS_13),
  (263858, &TOKENS_13),
  (263870, &TOKENS_13),
  (263882, &TOKENS_13),
  (263894, &TOKENS_13),
  (263906, &TOKENS_39),
  (264072, &TOKENS_76),
  (264132, &TOKENS_29),
  (264180, &TOKENS_64),
  (264722, &TOKENS_16),
  (264776, &TOKENS_39),
  (264851, &TOKENS_16),
  (264863, &TOKENS_16),
  (264875, &TOKENS_22),
  (264939, &TOKENS_74),
  (265017, &TOKENS_16),
  (265029, &TOKENS_74),
  (265107, &TOKENS_75),
  (265239, &TOKENS_13),
  (265251, &TOKENS_13),
  (265370, &TOKENS_13),
  (265496, &TOKENS_13),
  (265508, &TOKENS_13),
  (265520, &TOKENS_13),
  (265532, &TOKENS_13),
  (265544, &TOKENS_22),
  (265614, &TOKENS_17),
  (265662, &TOKENS_29),
  (265710, &TOKENS_64),
  (26622, &TOKENS_22),
  (266252, &TOKENS_16),
  (266305, &TOKENS_16),
  (266317, &TOKENS_16),
  (266329, &TOKENS_74),
  (266407, &TOKENS_75),
  (266539, &TOKENS_13),
  (266551, &TOKENS_13),
  (266670, &TOKENS_13),
  (266796, &TOKENS_13),
  (266808, &TOKENS_13),
  (266820, &TOKENS_13),
  (266832, &TOKENS_13),
  (266844, &TOKENS_79),
  (26689, &TOKENS_34),
  (267314, &TOKENS_17),
  (267362, &TOKENS_29),
  (267410, &TOKENS_64),
  (26764, &TOKENS_3),
  (267952, &TOKENS_16),
  (268005, &TOKENS_16),
  (268017, &TOKENS_16),
  (268029, &TOKENS_16),
  (268131, &TOKENS_22),
  (268313, &TOKENS_65),
  (268325, &TOKENS_22),
  (268400, &TOKENS_16),
  (268412, &TOKENS_29),
  (268460, &TOKENS_22),
  (268659, &TOKENS_0),
  (268775, &TOKENS_16),
  (268787, &TOKENS_16),
  (268799, &TOKENS_16),
  (268811, &TOKENS_64),
  (26928, &TOKENS_24),
  (269479, &TOKENS_60),
  (27003, &TOKENS_39),
  (27054, &TOKENS_89),
  (270688, &TOKENS_31),
  (270890, &TOKENS_83),
  (270971, &TOKENS_51),
  (271034, &TOKENS_22),
  (271090, &TOKENS_22),
  (271146, &TOKENS_31),
  (271158, &TOKENS_54),
  (271240, &TOKENS_36),
  (271275, &TOKENS_36),
  (27129, &TOKENS_39),
  (271310, &TOKENS_36),
  (271386, &TOKENS_36),
  (271458, &TOKENS_80),
  (271510, &TOKENS_22),
  (271567, &TOKENS_22),
  (271624, &TOKENS_36),
  (271659, &TOKENS_36),
  (271694, &TOKENS_36),
  (271729, &TOKENS_31),
  (271741, &TOKENS_22),
  (271797, &TOKENS_31),
  (27180, &TOKENS_95),
  (271809, &TOKENS_31),
  (271821, &TOKENS_29),
  (271879, &TOKENS_79),
  (27243, &TOKENS_109),
  (272469, &TOKENS_71),
  (27255, &TOKENS_9),
  (272676, &TOKENS_71),
  (273206, &TOKENS_71),
  (27324, &TOKENS_4),
  (273447, &TOKENS_31),
  (273601, &TOKENS_31),
  (273829, &TOKENS_53),
  (273910, &TOKENS_53),
  (273922, &TOKENS_29),
  (273980, &TOKENS_1),
  (274038, &TOKENS_16),
  (274164, &TOKENS_85),
  (274221, &TOKENS_85),
  (274233, &TOKENS_85),
  (274245, &TOKENS_53),
  (274257, &TOKENS_66),
  (274308, &TOKENS_22),
  (274370, &TOKENS_95),
  (274428, &TOKENS_56),
  (275505, &TOKENS_111),
  (275643, &TOKENS_53),
  (28172, &TOKENS_61),
  (28218, &TOKENS_34),
  (28293, &TOKENS_3),
  (2834, &TOKENS_104),
  (28581, &TOKENS_34),
  (28657, &TOKENS_3),
  (28797, &TOKENS_34),
  (28809, &TOKENS_9),
  (28878, &TOKENS_4),
  (29726, &TOKENS_5),
  (30556, &TOKENS_5),
  (31386, &TOKENS_5),
  (32216, &TOKENS_5),
  (33341, &TOKENS_6),
  (3348, &TOKENS_18),
  (3360, &TOKENS_119),
  (33676, &TOKENS_7),
  (33812, &TOKENS_5),
  (3453, &TOKENS_98),
  (34937, &TOKENS_7),
  (3502, &TOKENS_63),
  (35043, &TOKENS_5),
  (35873, &TOKENS_5),
  (3602, &TOKENS_63),
  (3656, &TOKENS_96),
  (36998, &TOKENS_7),
  (37104, &TOKENS_5),
  (3745, &TOKENS_98),
  (3793, &TOKENS_63),
  (37934, &TOKENS_5),
  (3894, &TOKENS_63),
  (39059, &TOKENS_7),
  (39189, &TOKENS_5),
  (3949, &TOKENS_76),
  (3991, &TOKENS_43),
  (40019, &TOKENS_5),
  (41144, &TOKENS_7),
  (41274, &TOKENS_5),
  (42104, &TOKENS_5),
  (43229, &TOKENS_7),
  (43335, &TOKENS_5),
  (44165, &TOKENS_5),
  (45290, &TOKENS_7),
  (45360, &TOKENS_5),
  (46190, &TOKENS_5),
  (4638, &TOKENS_76),
  (4650, &TOKENS_97),
  (47020, &TOKENS_5),
  (48166, &TOKENS_6),
  (48296, &TOKENS_5),
  (49421, &TOKENS_6),
  (49527, &TOKENS_5),
  (4969, &TOKENS_67),
  (50357, &TOKENS_5),
  (5088, &TOKENS_67),
  (51482, &TOKENS_6),
  (51588, &TOKENS_5),
  (52418, &TOKENS_5),
  (53543, &TOKENS_6),
  (53649, &TOKENS_5),
  (5383, &TOKENS_67),
  (5395, &TOKENS_92),
  (5407, &TOKENS_97),
  (54479, &TOKENS_5),
  (55604, &TOKENS_6),
  (55674, &TOKENS_5),
  (56504, &TOKENS_5),
  (5726, &TOKENS_67),
  (57629, &TOKENS_6),
  (57759, &TOKENS_5),
  (5845, &TOKENS_67),
  (58589, &TOKENS_5),
  (59714, &TOKENS_6),
  (59844, &TOKENS_5),
  (60674, &TOKENS_34),
  (60686, &TOKENS_5),
  (6140, &TOKENS_67),
  (61516, &TOKENS_5),
  (6152, &TOKENS_119),
  (6164, &TOKENS_32),
  (6200, &TOKENS_92),
  (6212, &TOKENS_110),
  (6224, &TOKENS_92),
  (62346, &TOKENS_5),
  (6317, &TOKENS_107),
  (63176, &TOKENS_5),
  (6353, &TOKENS_107),
  (64301, &TOKENS_7),
  (64437, &TOKENS_5),
  (65267, &TOKENS_5),
  (6555, &TOKENS_18),
  (66097, &TOKENS_5),
  (66927, &TOKENS_5),
  (68052, &TOKENS_7),
  (68158, &TOKENS_5),
  (68988, &TOKENS_5),
  (70113, &TOKENS_7),
  (70219, &TOKENS_5),
  (7069, &TOKENS_104),
  (7081, &TOKENS_42),
  (7093, &TOKENS_2),
  (71049, &TOKENS_5),
  (7105, &TOKENS_100),
  (7156, &TOKENS_22),
  (7212, &TOKENS_56),
  (72174, &TOKENS_7),
  (72304, &TOKENS_5),
  (73134, &TOKENS_5),
  (74259, &TOKENS_7),
  (74389, &TOKENS_5),
  (75219, &TOKENS_5),
  (76344, &TOKENS_7),
  (76450, &TOKENS_5),
  (77280, &TOKENS_5),
  (78405, &TOKENS_7),
  (78475, &TOKENS_5),
  (79305, &TOKENS_5),
  (80135, &TOKENS_5),
  (80965, &TOKENS_69),
  (81052, &TOKENS_22),
  (81119, &TOKENS_6),
  (81447, &TOKENS_6),
  (81577, &TOKENS_5),
  (82407, &TOKENS_5),
  (8289, &TOKENS_17),
  (83237, &TOKENS_5),
  (8352, &TOKENS_123),
  (84067, &TOKENS_5),
  (85192, &TOKENS_6),
  (85298, &TOKENS_5),
  (86128, &TOKENS_5),
  (87253, &TOKENS_6),
  (87323, &TOKENS_5),
  (88153, &TOKENS_5),
  (89278, &TOKENS_6),
  (89384, &TOKENS_5),
  (90214, &TOKENS_5),
  (91339, &TOKENS_6),
  (91469, &TOKENS_5),
  (92299, &TOKENS_5),
  (9337, &TOKENS_45),
  (93424, &TOKENS_6),
  (93554, &TOKENS_5),
  (9388, &TOKENS_41),
  (94384, &TOKENS_5),
  (9445, &TOKENS_105),
  (95509, &TOKENS_6),
  (95615, &TOKENS_5),
  (96445, &TOKENS_24),
  (96520, &TOKENS_109),
  (96532, &TOKENS_24),
  (96607, &TOKENS_109),
  (96743, &TOKENS_9),
  (96812, &TOKENS_4),
  (97660, &TOKENS_5),
  (9835, &TOKENS_25),
  (98490, &TOKENS_5),
  (9916, &TOKENS_91),
  (99320, &TOKENS_5),
  (9985, &TOKENS_108),
];

const TOKENS_0: [u32;2]=[9,65,];

const TOKENS_1: [u32;1]=[63,];

const TOKENS_10: [u32;15]=[9,9,11,12,12,13,17,17,43,44,45,46,47,48,81,];

const TOKENS_100: [u32;30]=[
  8,
  9,
  11,
  12,
  19,
  20,
  44,
  45,
  47,
  59,
  63,
  65,
  74,
  75,
  78,
  80,
  84,
  114,
  115,
  116,
  117,
  118,
  119,
  120,
  121,
  122,
  123,
  124,
  133,
  134,
];

const TOKENS_101: [u32;2]=[3,4,];

const TOKENS_102: [u32;24]=[9,11,12,20,47,59,63,65,74,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_103: [u32;24]=[9,11,12,20,47,59,63,65,74,75,78,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_104: [u32;14]=[11,12,20,48,80,84,93,116,117,136,137,138,139,140,];

const TOKENS_105: [u32;23]=[9,11,12,20,47,59,63,65,74,79,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_106: [u32;39]=[
  3,
  4,
  8,
  9,
  11,
  12,
  19,
  20,
  44,
  45,
  47,
  51,
  52,
  53,
  57,
  58,
  59,
  63,
  65,
  74,
  75,
  78,
  80,
  81,
  82,
  84,
  114,
  115,
  116,
  117,
  118,
  119,
  120,
  121,
  122,
  123,
  124,
  133,
  134,
];

const TOKENS_107: [u32;1]=[47,];

const TOKENS_108: [u32;18]=[11,47,65,74,75,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_109: [u32;3]=[9,12,17,];

const TOKENS_11: [u32;2]=[9,13,];

const TOKENS_110: [u32;20]=[8,11,12,20,44,45,47,48,75,80,81,84,93,116,117,136,137,138,139,140,];

const TOKENS_111: [u32;3]=[9,59,63,];

const TOKENS_112: [u32;25]=[9,11,12,20,47,59,63,65,74,78,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_113: [u32;25]=[8,9,11,12,20,47,59,63,65,74,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_114: [u32;3]=[34,36,37,];

const TOKENS_115: [u32;2]=[17,81,];

const TOKENS_116: [u32;16]=[9,9,12,12,13,17,17,43,44,45,46,47,48,81,92,93,];

const TOKENS_117: [u32;2]=[23,67,];

const TOKENS_118: [u32;4]=[9,40,126,127,];

const TOKENS_119: [u32;18]=[8,11,12,20,44,45,48,75,80,84,93,116,117,136,137,138,139,140,];

const TOKENS_12: [u32;3]=[8,11,40,];

const TOKENS_120: [u32;1]=[21,];

const TOKENS_121: [u32;3]=[65,87,88,];

const TOKENS_122: [u32;27]=[8,9,11,12,20,47,59,63,65,74,75,78,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_123: [u32;15]=[11,47,65,74,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_124: [u32;3]=[27,65,67,];

const TOKENS_13: [u32;5]=[8,9,11,128,129,];

const TOKENS_14: [u32;8]=[17,43,44,45,46,47,48,81,];

const TOKENS_15: [u32;26]=[8,9,11,12,20,47,59,63,65,74,78,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_16: [u32;1]=[9,];

const TOKENS_17: [u32;1]=[12,];

const TOKENS_18: [u32;15]=[11,12,20,47,48,80,84,93,116,117,136,137,138,139,140,];

const TOKENS_19: [u32;4]=[12,47,133,134,];

const TOKENS_2: [u32;29]=[8,9,11,12,20,44,45,47,59,63,65,74,75,78,80,84,114,115,116,117,118,119,120,121,122,123,124,133,134,];

const TOKENS_20: [u32;2]=[34,36,];

const TOKENS_21: [u32;8]=[9,13,43,44,45,46,47,48,];

const TOKENS_22: [u32;1]=[65,];

const TOKENS_23: [u32;1]=[11,];

const TOKENS_24: [u32;4]=[9,12,17,92,];

const TOKENS_25: [u32;20]=[8,11,47,65,74,75,78,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_26: [u32;3]=[9,13,24,];

const TOKENS_27: [u32;2]=[89,90,];

const TOKENS_28: [u32;14]=[9,9,12,12,13,17,17,43,44,45,46,47,48,81,];

const TOKENS_29: [u32;1]=[8,];

const TOKENS_3: [u32;4]=[34,65,71,72,];

const TOKENS_30: [u32;2]=[9,40,];

const TOKENS_31: [u32;5]=[53,57,58,59,63,];

const TOKENS_32: [u32;1]=[137,];

const TOKENS_33: [u32;5]=[9,12,20,59,63,];

const TOKENS_34: [u32;2]=[9,17,];

const TOKENS_35: [u32;23]=[9,11,12,20,47,59,63,65,74,75,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_36: [u32;4]=[1,54,55,56,];

const TOKENS_37: [u32;12]=[25,26,28,29,33,68,69,70,85,89,90,91,];

const TOKENS_38: [u32;19]=[8,11,47,65,74,78,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_39: [u32;1]=[67,];

const TOKENS_4: [u32;26]=[8,11,46,80,84,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,132,133,];

const TOKENS_40: [u32;7]=[68,69,70,85,89,90,91,];

const TOKENS_41: [u32;2]=[81,82,];

const TOKENS_42: [u32;31]=[
  8,
  9,
  11,
  12,
  20,
  44,
  45,
  47,
  59,
  63,
  65,
  74,
  75,
  78,
  80,
  81,
  82,
  84,
  114,
  115,
  116,
  117,
  118,
  119,
  120,
  121,
  122,
  123,
  124,
  133,
  134,
];

const TOKENS_43: [u32;12]=[11,48,80,84,93,116,117,136,137,138,139,140,];

const TOKENS_44: [u32;7]=[17,43,44,45,46,47,48,];

const TOKENS_45: [u32;25]=[8,11,19,44,45,47,65,74,75,78,81,82,114,115,116,117,118,119,120,121,122,123,124,133,134,];

const TOKENS_46: [u32;24]=[8,9,11,12,20,47,59,63,65,74,78,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_47: [u32;25]=[8,9,11,12,20,47,59,63,65,74,75,78,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_48: [u32;5]=[12,17,47,133,134,];

const TOKENS_49: [u32;9]=[9,9,13,43,44,45,46,47,48,];

const TOKENS_5: [u32;25]=[11,46,80,84,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,132,133,];

const TOKENS_50: [u32;1]=[40,];

const TOKENS_51: [u32;7]=[51,52,53,57,58,59,63,];

const TOKENS_52: [u32;18]=[11,47,65,67,74,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_53: [u32;2]=[59,63,];

const TOKENS_54: [u32;3]=[54,55,56,];

const TOKENS_55: [u32;26]=[9,11,12,20,47,59,63,65,74,75,78,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_56: [u32;17]=[11,47,65,74,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_57: [u32;23]=[8,9,11,12,20,47,59,63,65,74,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_58: [u32;24]=[8,9,11,12,20,47,59,63,65,74,75,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_59: [u32;18]=[8,11,47,65,74,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_6: [u32;8]=[9,17,43,44,45,46,47,48,];

const TOKENS_60: [u32;27]=[8,11,42,46,80,84,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,132,133,];

const TOKENS_61: [u32;1]=[41,];

const TOKENS_62: [u32;1]=[133,];

const TOKENS_63: [u32;2]=[9,135,];

const TOKENS_64: [u32;15]=[25,26,28,29,33,35,38,39,68,69,70,85,89,90,91,];

const TOKENS_65: [u32;1]=[13,];

const TOKENS_66: [u32;6]=[8,53,57,58,59,63,];

const TOKENS_67: [u32;8]=[81,93,116,117,137,138,139,140,];

const TOKENS_68: [u32;19]=[11,47,65,74,75,78,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_69: [u32;9]=[12,43,44,45,46,47,48,92,93,];

const TOKENS_7: [u32;7]=[12,43,44,45,46,47,48,];

const TOKENS_70: [u32;24]=[8,11,44,45,47,65,74,75,78,81,82,114,115,116,117,118,119,120,121,122,123,124,133,134,];

const TOKENS_71: [u32;14]=[9,47,74,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_72: [u32;3]=[9,126,127,];

const TOKENS_73: [u32;8]=[34,71,72,84,112,113,132,133,];

const TOKENS_74: [u32;2]=[8,11,];

const TOKENS_75: [u32;4]=[8,11,128,129,];

const TOKENS_76: [u32;2]=[12,20,];

const TOKENS_77: [u32;1]=[22,];

const TOKENS_78: [u32;25]=[9,11,12,20,47,59,63,65,74,75,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_79: [u32;13]=[47,74,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_8: [u32;22]=[9,11,12,20,47,59,63,65,74,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_80: [u32;2]=[51,52,];

const TOKENS_81: [u32;18]=[11,47,65,74,78,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_82: [u32;2]=[9,126,];

const TOKENS_83: [u32;2]=[11,65,];

const TOKENS_84: [u32;23]=[9,11,12,20,47,59,63,65,74,78,80,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_85: [u32;2]=[9,63,];

const TOKENS_86: [u32;11]=[9,9,13,43,44,45,46,47,48,92,93,];

const TOKENS_87: [u32;26]=[8,9,11,12,20,47,59,63,65,74,75,80,81,82,84,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_88: [u32;26]=[11,41,46,80,84,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,132,133,];

const TOKENS_89: [u32;2]=[17,62,];

const TOKENS_9: [u32;3]=[9,17,40,];

const TOKENS_90: [u32;1]=[19,];

const TOKENS_91: [u32;19]=[8,11,47,65,74,75,81,82,114,115,116,117,118,119,120,121,122,133,134,];

const TOKENS_92: [u32;19]=[8,11,12,20,44,45,47,48,75,80,84,93,116,117,136,137,138,139,140,];

const TOKENS_93: [u32;26]=[11,46,80,81,84,94,95,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,132,133,];

const TOKENS_94: [u32;2]=[65,67,];

const TOKENS_95: [u32;1]=[62,];

const TOKENS_96: [u32;3]=[9,17,135,];

const TOKENS_97: [u32;7]=[93,116,117,137,138,139,140,];

const TOKENS_98: [u32;2]=[17,135,];

const TOKENS_99: [u32;17]=[11,47,65,74,81,82,114,115,116,117,118,119,120,121,122,133,134,];


/// Parser database for the "" parser
pub struct ParserDB {
  pub bytecode: &'static [u8],
  pub nonterm_name_to_id: HashMap<&'static str, u32>,
  pub state_to_token_ids_map: HashMap<u32, &'static [u32]>,
  pub nonterm_id_to_address: HashMap<u32, u32>,
  pub token_id_to_str: HashMap<u32, &'static str>,

}

impl ParserDB {
  pub fn new() -> Self {Self {
      bytecode: BINARY,
      nonterm_name_to_id: HashMap::from_iter(NONTERM_NAME_TO_ID),
      state_to_token_ids_map: HashMap::from_iter(STATE_TO_TOKEN_IDS),
      nonterm_id_to_address: HashMap::from_iter(NONTERM_ID_TO_ADDRESS),
      token_id_to_str: HashMap::from_iter(TOKEN_ID_TO_STRING)
    
    }
  
  }

}

impl AsRef<[u8]> for ParserDB {
  fn as_ref(&self) -> &[u8] {
    self.bytecode
  }
}


impl RuntimeDatabase for ParserDB {
  fn default_entrypoint(&self) -> EntryPoint {
      EntryPoint { nonterm_id: 0 }
  }

  fn get_entry_data_from_name(&self, entry_name: &str) -> Result<EntryPoint, ParserError> {
    if let Some(id) = self.nonterm_name_to_id.get(entry_name) {
      Ok(EntryPoint { nonterm_id: *id })
    } else {
      Err(ParserError::InvalidEntryName)
    }
  
  }

  fn get_expected_tok_ids_at_state(&self, state_id: u32) -> Option<&[u32]> {
    self.state_to_token_ids_map.get(&state_id).map(|s| *s)
  }

  fn token_id_to_str(&self, tok_id: u32) -> Option<&str> {
    self.token_id_to_str.get(&tok_id).map(|s| *s)
  }

  fn entrypoints(&self) -> Vec<(std::string::String, u32)> {
    vec![]
  }

}

impl<T: ParserInput> ParserProducer<T> for ParserDB {
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParserError> {
    Ok(Box::new(ByteCodeParserNew::new(Rc::new(self.bytecode), self.nonterm_id_to_address.clone())))
  
  }

}


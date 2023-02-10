//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `Resource0` derive macros to
//! automatically interface with game files and the SQL database.
//!
//! **Most** of the Lua-side definition of resource schemas is also
//! derived from here. The exception is the relation between resources
//! and their subresources. For now, this is inserted by hand by the
//! `init.lua` file.
//!
//! The tables share the following common structure:
//! 1. Payload fields: these carry actual game information and
//!    and are defined in game resources. These always come as the first
//!    columns so that `select` statements can load resource fields from
//!    predictible column indices.
//! 2. Context fields: these identify in which game resource, and where,
//!    this information is located. The first context field is always
//!    the resref for the main resource.
//!    These fields also carry a `unique` constraint.
//! 3. For subresources: a `rowid` alias (`integer primary key`).
//!    (Top-level resources don't need this:
//!    they have the resref as their primary key).
//!
//! The `integer primary key` is needed even for subresources:
//! namely, we want these to have a persistent identifier
//! for joins with the edit tables.
use crate::prelude::*;
use macros::{produce_resource_list,all_resources,Resource};
use crate::struct_io::{Pack,SqlRow,NotPacked,NoSql};
use crate::schemas::{Schema,Resource,Schema0};
use crate::database::{DbTypeCheck,DbInterface,DbInserter,TypedStatement};
use crate::gamefiles::{Restype};

#[derive(Pack)]
struct Repack<T>(T);

/// Interface for a game resource.
///
/// This trait is implemented by the corresponding derive macro.
///
/// This is the main trait for game resources, containing the low-level
/// interaction with the database (`ins`, `sel`). Concrete
/// implementations are provided by the `Resource0` derive macro.
pub trait Resource0: Sized {
	/// Reads a whole [`rusqlite::Row`] into a struct,
	/// or raises a conversion error.
// 	fn from_row(r: &rusqlite::Row<'_>)->Result<Self>;
	/// Additional data saved in the same row as a game object; usually
	/// some identifier for the object (e.g. its resref).
	type Context;
	/// The internal description of the fields of this structure, as used
	/// to produce SQL statements.
	const SCHEMA: Schema0<'static>;
	/// The low-level insert function for an object to a database row.
	fn ins(&self, s: &mut Statement<'_>, key: &Self::Context)->rusqlite::Result<()>;
	/// The low-level select function from a database row to an object.
	fn sel(r: &rusqlite::Row<'_>)->Result<(Self, Self::Context)>;
	/// The select Statement for an object.
	///
	/// The ID field (if present) is not selected. (This is used only for
	/// building game files, where this field is not used anyway).
	///
	/// The name of the table is `{n1}{n2}`; since many tables are built in
	/// two parts (e.g. `load_{name}`, `strings_{lang}`),
	/// this avoids calling `format!` on the
	/// caller side and slightly simplifies the API.
	///
	/// This returns a [`Result<TypedStatement>`]: it is possible to iterate
	/// over the result and recover structures of the original type.
	fn select_query_gen(db: &impl DbInterface, n1: impl Display, n2: impl Display, cond: impl Display)->Result<TypedStatement<'_,Self>> {
		let s = Self::SCHEMA.select_statement(n1, n2, cond);
		Ok(TypedStatement::new(db.prepare(&s)?))
	}
	/// Particular case of SELECT statement used for saving to game files.
	fn select_query(db: &impl DbInterface, s: impl Display)->Result<TypedStatement<'_, Self>> {
		Self::select_query_gen(db, "save_", Self::SCHEMA, s)
	}
}
/// Those resources which are associated to a global table.
pub trait NamedTable: Resource0 {
	/// Returns the associated field (e.g. `.item`) in an `AllResources0` table.
	fn find_field<T: Debug>(all: &AllResources0<T>)->&T;
	/// Same as above; `mut` case.
	fn find_field_mut<T: Debug>(all: &mut AllResources0<T>)->&mut T;
}
/// When provided with a file extension, return the corresponding Restype.
pub fn restype_from_extension(ext: &str)->Restype {
	match RESOURCES.map(|schema, _| {
		// small trick: when we find the correct extension, we break the
		// iteration by returning an `Err` variant.
		if ext.eq_ignore_ascii_case(schema.extension) { Err(schema.restype) }
		else { Ok(()) }
	}) {
		Err(r) => r,
		Ok(_) => Restype { value: 0 }
	}
}

/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,Resource0)]
#[allow(missing_copy_implementations)]
#[resource(item_effects)] pub struct ItemEffect {
#[column(itemref, Resref, r#"references "items"("itemref") on delete cascade"#)]
#[column(abref, usize, r#"references "item_abilities"("abref") on delete cascade"#)]
// itemref is used only for sorting:
#[column(effectref, usize)]
	opcode: u16, //opcode,
	target: u8, // EffectTarget,
	power: u8,
	parameter1: u32,
	parameter2: u32,
	timing_mode: u8, // TimingMode,
	dispel_mode: u8, // DispelMode,
	duration: u32,
	proba1: u8,
	proba2: u8,
	resource: Resref,
	dice_thrown: i32,
	dice_sides: i32,
	saving_throw_type: u32,
	saving_throw_bonus: i32,
	stacking_id: u32,
}
/// An ability inside a .itm file.
#[derive(Debug,Pack,Resource0)]
#[allow(missing_copy_implementations)]
#[resource(item_abilities)] pub struct ItemAbility {
#[column(itemref, Resref, r#"references "items"("itemref") on delete cascade"#)]
#[column(abref, usize)]
	attack_type: u8, // AttackType,
	must_identify: u8,
	location: u8,
	alternative_dice_sides: u8,
	use_icon: Resref,
	target_type: u8, // TargetType,
	target_count: u8,
	range: u16,
	launcher_required: u8,
	alternative_dice_thrown: u8,
	speed_factor: u8,
	alternative_damage_bonus: u8,
	thac0_bonus: u16,
	dice_sides: u8,
	primary_type: u8,
	dice_thrown: u8,
	secondary_type: u8,
	damage_bonus: u16,
	damage_type: u16, // DamageType,
#[column(false)] effect_count: u16, // = 0,
#[column(false)] effect_index: u16, // = 0,
	max_charges: u16,
	depletion: u16,
	flags: u32,
	projectile_animation: u16,
	overhand_chance: u16,
	backhand_chance: u16,
	thrust_chance: u16,
	is_arrow: u16,
	is_bolt: u16,
	is_bullet: u16,
}
/// An item effect, corresponding to a .itm file.
#[derive(Debug,Pack,Resource0)]
#[allow(missing_copy_implementations)]
#[resource(items,"itm",0x03ed)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref)]
	unidentified_name: Strref,
	name: Strref,
	replacement: Resref,
	flags: u32, // ItemFlags,
	itemtype: u16, // ItemType,
	usability: u32, // UsabilityFlags,
	animation: u16, // StaticString<2>,
	min_level: u16,
	min_strength: u16,
	min_strengthbonus: u8,
	kit1: u8,
	min_intelligence: u8,
	kit2: u8,
	min_dexterity: u8,
	kit3: u8,
	min_wisdom: u8,
	kit4: u8,
	min_constitution: u8,
	proficiency: u8, // WProf,
	min_charisma: u16,
	price: u32,
	stack_amount: u16,
	inventory_icon: Resref,
	lore: u16,
	ground_icon: Resref,
	weight: i32,
	unidentified_description: Strref,
	description: Strref,
	description_icon: Resref,
	enchantment: i32,
#[column(false)] abilities_offset: u32,
#[column(false)] abilities_count: u16,
#[column(false)] effect_offset: u32,
#[column(false)] effect_index: u16,
#[column(false)] equip_effect_count: u16,
}
/// A game item, corresponding to a .itm file.
#[derive(Pack,SqlRow,Resource)]
#[topresource("items", "itm", 0x03ed)]
pub struct Item1 {
#[header("ITM V1  ")]
	unidentified_name: Strref,
	name: Strref,
	replacement: Resref,
	flags: u32, // ItemFlags,
	itemtype: u16, // ItemType,
	usability: u32, // UsabilityFlags,
	animation: u16, // StaticString<2>,
	min_level: u16,
	min_strength: u16,
	min_strengthbonus: u8,
	kit1: u8,
	min_intelligence: u8,
	kit2: u8,
	min_dexterity: u8,
	kit3: u8,
	min_wisdom: u8,
	kit4: u8,
	min_constitution: u8,
	proficiency: u8, // WProf,
	min_charisma: u16,
	price: u32,
	stack_amount: u16,
	inventory_icon: Resref,
	lore: u16,
	ground_icon: Resref,
	weight: i32,
	unidentified_description: Strref,
	description: Strref,
	description_icon: Resref,
	enchantment: i32,
	abilities_offset: NoSql::<u32>,
	abilities_count: NoSql::<u16>,
	effect_offset: NoSql::<u32>,
	effect_index: NoSql::<u16>,
	equip_effect_count: NoSql::<u16>,
#[column("primary key")]
	itemref: NotPacked::<Resref>,
}
/// An ability inside a .itm file.
#[derive(Debug,Pack,SqlRow)]
#[allow(missing_copy_implementations)]
// #[resource(item_abilities)]
pub struct ItemAbility1 {
	attack_type: u8, // AttackType,
	must_identify: u8,
	location: u8,
	alternative_dice_sides: u8,
	use_icon: Resref,
	target_type: u8, // TargetType,
	target_count: u8,
	range: u16,
	launcher_required: u8,
	alternative_dice_thrown: u8,
	speed_factor: u8,
	alternative_damage_bonus: u8,
	thac0_bonus: u16,
	dice_sides: u8,
	primary_type: u8,
	dice_thrown: u8,
	secondary_type: u8,
	damage_bonus: u16,
	damage_type: u16, // DamageType,
	effect_count: NoSql::<u16>, // = 0,
	effect_index: NoSql::<u16>, // = 0,
	max_charges: u16,
	depletion: u16,
	flags: u32,
	projectile_animation: u16,
	overhand_chance: u16,
	backhand_chance: u16,
	thrust_chance: u16,
	is_arrow: u16,
	is_bolt: u16,
	is_bullet: u16,
#[column(r#"references "items"("itemref") on delete cascade"#)]
	itemref: NotPacked::<Resref>,
	index: NotPacked::<u16>,
#[column(r#"primary key"#)]
	id: NotPacked::<Option<i64>>,
}
/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow)]
#[allow(missing_copy_implementations)]
// #[resource(item_effects)]
pub struct ItemEffect1 {
	opcode: u16, //opcode,
	target: u8, // EffectTarget,
	power: u8,
	parameter1: u32,
	parameter2: u32,
	timing_mode: u8, // TimingMode,
	dispel_mode: u8, // DispelMode,
	duration: u32,
	proba1: u8,
	proba2: u8,
	resource: Resref,
	dice_thrown: i32,
	dice_sides: i32,
	saving_throw_type: u32,
	saving_throw_bonus: i32,
	stacking_id: u32,
	itemref: NotPacked::<Resref>,
#[column(r#"references "item_abilities"("id") on delete cascade"#)]
	ability: NotPacked::<i64>,
	index: NotPacked::<u16>,
#[column(r#"primary key"#)]
	id: NotPacked::<Option<i64>>,
}

/// A top-level resource (i.e. saved to its own file in the override
/// directory).
pub trait ToplevelResource: NamedTable {
	/// Subresources for this resource (together with linking info.).
	///
	/// This should be a tuple of all subresources, each of them stored in
	/// some `Vec` with elements (subresource, linking-info).
	type Subresources<'a>;
	/// Inserts a single resource in the database.
	fn load(db: &mut DbInserter<'_>, cursor: impl Read+Seek, resref: Resref)
		->Result<()>;
	/// Saves a single resource (with its subresources) to game files.
	fn save(&mut self, file: impl Write+Seek+Debug, subresources: Self::Subresources<'_>)
		->Result<()>;
	/// Applies a closure over all resources together with their subresources.
	///
	/// Iterates a closure over items in the database (with their
	/// associated properties) matching the given condition.
	///
	/// This function links items with their sub-resources
	/// (e.g. item abilities and effects)
	/// and calls the passed closure on the resulting structure.
	///
	/// It returns the number of rows matched.
	fn for_each<F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>>
		(db: &impl DbInterface, condition: impl Display, f:F)->Result<i32>;
	/// Saves all toplevel resources of this type.
	fn save_all(db: &impl DbInterface)->Result<()> {
		let Schema0 { name, extension, .. } = Self::SCHEMA;
		scope_trace!("saving resources from '{}'", name);
		let condition = format!(r#"where "key" in "dirty_{name}""#);
		let n = Self::for_each(db, condition,
			|resref, resource, subresources| {
				trace!("start compiling {name}: {resref}");
				let filename = format!("{resref}.{extension}");
				let mut file = File::create(&filename)
					.with_context(|| format!("cannot open output file: {filename}"))?;
				resource.save(&mut file, subresources)
		})?;
		info!("compiled {n} entries from {name} to override");
		Ok(())
	}
	/// Shows a resource on stdout (grouped with its subresources) in a
	/// nice user format.
	fn show(&self, resref: impl Display, subresources: Self::Subresources<'_>);
	/// Shows on stdout the resource with given resref, or returns an error.
	fn show_all(db: &impl DbInterface, resref: impl Display)->Result<()> {
		Self::for_each(db, format!(r#"where "id"='{resref}'"#),
		|resref, resource, subresources| {
			resource.show(resref, subresources);
			Ok(())
		}).unwrap();
		Ok(())
	}
} // trait ToplevelResource
impl ToplevelResource for Item {
	type Subresources<'a> = (&'a mut [ItemAbility], &'a mut[(ItemEffect,usize)]);
	/// load an item from cursor
	fn load(db: &mut DbInserter<'_>, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let mut item = Item1::unpack(&mut cursor)
			.context("cannot unpack Item main struct")?;
		item.itemref = resref.into();
		db.tables.items.execute(item.as_params())
			.context("inserting into 'items'")?;
		cursor.seek(SeekFrom::Start(item.abilities_offset.unwrap() as u64))?;

		let mut ab_n = Vec::<u16>::
			with_capacity(item.abilities_count.unwrap() as usize);
		for ab_index in 1..1+item.abilities_count.unwrap() {
			let mut ab = ItemAbility1::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack item ability {}/{}",
					ab_index, item.abilities_count))?;
			ab.itemref = resref.into();
			ab.index = ab_index.into();
			db.tables.item_abilities.execute(ab.as_params())
				.context("inserting into 'item_abilities'")?;
			ab_n.push(ab.effect_count.unwrap());
		}
		trace!("inserting item {resref}; abilities have {ab_n:?} effects");
		cursor.seek(SeekFrom::Start(item.effect_offset.unwrap() as u64))?;
		for j in 1..1+item.equip_effect_count.unwrap() { // on-equip effects
			let mut eff = ItemEffect1::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack global item effect {}/{}",
					j+1, item.equip_effect_count.unwrap()))?;
			eff.itemref = resref.into();
			eff.ability = 0.into();
			eff.index = j.into();
			db.tables.item_effects.execute(eff.as_params())
				.context("inserting into 'item_effects'")?;
		}
		for (i, n) in ab_n.iter().enumerate() {
			for j in 1..(1+*n) {
				let mut eff = ItemEffect1::unpack(&mut cursor)
					.with_context(|| format!("cannot unpack item effect {}/{} for ability {}", j+1, n, i+1))?;
				eff.itemref = resref.into();
				eff.ability = (1+i as i64).into();
				eff.index = j.into();
				db.tables.item_effects.execute(eff.as_params())
					.context("inserting into 'item_effects'")?;
			}
		}
	Ok(())
	}
	/// saves an item (with its sub-resources) to a file
	fn save(&mut self, mut file: impl Write+Seek+Debug, (abilities, effects): Self::Subresources<'_>) ->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			abilities.len(), effects.len());
		let mut current_effect_idx = self.equip_effect_count;
		for a in abilities.iter_mut() {
			a.effect_index = current_effect_idx;
			current_effect_idx+= a.effect_count;
		}
		self.abilities_offset = 114;
		self.effect_offset = 114 + 56*(self.abilities_count as u32);
		println!("\x1b[34mitem is: {self:?}\x1b[m");

		self.pack(&mut file)
			.with_context(|| format!("cannot pack item to {file:?}"))?;
		// pack abilities:
		for a in abilities.iter() {
			a.pack(&mut file)
			.with_context(|| format!("cannot pack item ability to {file:?}"))?;
		}
		// pack effects: first on-equip, then grouped by ability
		for (e, j) in effects.iter() {
			if *j == 0 {
				e.pack(&mut file)?;
			}
		}
		for i in 0..self.abilities_count {
			for (e, j) in effects.iter() {
				if *j == (i+1).into() {
					e.pack(&mut file)?;
				}
			}
		}
		Ok(())
	}
	fn for_each<F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>>
		(db: &impl DbInterface, condition: impl Display, f:F)->Result<i32> {
		let mut sel_item = Item::select_query(db, &condition)?;
		let mut sel_item_ab = ItemAbility::select_query(db, r#"where "key"=?"#)?;
		let mut sel_item_eff = ItemEffect::select_query(db, r#"where "key"=?"#)?;
		let mut n_items = 0;
		debug!("processing items under condition: {condition}");
		for x in sel_item.iter(())? {
			if x.is_db_malformed() { continue }
			let (mut item, (itemref,)) = x?;
			debug!("reading item: {itemref}");
			// we store item effects & abilities in two vectors:
			//  - abilities = (ability, abref)
			//  - effect = (effect, ability index)
			let mut abilities = Vec::<ItemAbility>::new();
			let mut effects = Vec::<(ItemEffect, usize)>::new();
			for x in sel_item_ab.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (ab, _) = x?;
				abilities.push(ab);
				item.abilities_count+= 1;
			}
			for x in sel_item_eff.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (eff, (_parent, ab_id, _eff_id)) = x?;
				if ab_id > 0 {
					abilities[ab_id-1].effect_count+=1;
				} else {
					item.equip_effect_count+= 1;
				}
				effects.push((eff, ab_id));
			}
			f(itemref, &mut item, (&mut abilities, &mut effects))?;
			n_items+= 1;
		}
		debug!("processed {n_items} items");
		Ok(n_items)
	}
	fn show(&self, itemref: impl Display, (abilities, _effects): Self::Subresources<'_>) {
		println!("\
[{itemref}] {name}/{unidentified_name} ${price} ⚖{weight} ?{lore}
Requires: {st}/{ste} St, {dx} Dx, {co} Co, {wi} Wi, {in} In, {ch} Cha
Proficiency: {prof}
", name=self.name, unidentified_name = self.unidentified_name,
	price=self.price, weight=self.weight, lore=self.lore,
	st=self.min_strength, ste=self.min_strengthbonus,
	dx=self.min_dexterity, co=self.min_constitution,
	wi=self.min_wisdom, in=self.min_intelligence, ch=self.min_charisma,
	prof=self.proficiency);
		for ability in abilities {
			println!("
Attack type: {atype}",
			atype=ability.attack_type);
		}
	}
}

// This last invocation closes the list of resources above, generating:
//  - the `AllResource<T>` type constructor,
//  - its implementation of `map()`,
//  - and the constant `RESOURCES`, which holds the parent resources.
produce_resource_list!();
all_resources!();

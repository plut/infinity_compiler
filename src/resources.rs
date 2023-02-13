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
use macros::{all_resources,Resource};
use crate::pack::{Pack,NotPacked};
use crate::sql_rows::{SqlRow,NoSql,TypedStatement,AsParams,Rowid};
use crate::schemas::{Schema};
use crate::database::{DbTypeCheck,DbInterface,DbInserter};
use crate::gamefiles::{Restype};

/// One of the resource tables stored in the database.
///
/// Methods attached to this trait are those which depend **both** on
///  - having a full schema (with table name, etc.), and
///  - having access to the attached (Rust) type (for typed inserts,
///  etc.).
/// Methods depending only on the list of columns of the schema and the
/// Rust struct go to [`SqlRow`].
/// Methods depending only on the schema go to [`Schema`].
/// Methods depending only on the list of columns from the schema (i.e.
/// the intersection of both cases) go to [`ColumnWriter`]
pub trait Resource: SqlRow {
	fn schema()->Schema;
	/// Particular case of SELECT statement used for saving to game files.
	fn select_typed(db: &impl DbInterface, s: impl Display)->Result<TypedStatement<'_, Self>> {
		Self::select_from_typed(db, lazy_format!("save_{name}", name=Self::schema().name), s)
	}
}

/// When provided with a file extension, return the corresponding Restype.
pub fn restype_from_extension(ext: &str)->Restype {
	match all_schemas().map(|schema| {
		match schema.resource {
		// small trick: when we find the correct extension, we break the
		// iteration by returning an `Err` variant.
			crate::schemas::SchemaResource::Top { extension, restype }
			if ext.eq_ignore_ascii_case (extension) => Err(restype),
			_ => Ok(())
		}
	}) {
		Err(r) => r,
		Ok(_) => Restype { value: 0 }
	}
}

/// A game item, corresponding to a .itm file.
#[derive(Debug,Pack,SqlRow,Resource)]
#[topresource("items", "itm", 0x03ed)]
pub struct Item {
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
#[derive(Debug,Pack,SqlRow,Resource)]
#[subresource(item_abilities,itemref,items)]
#[allow(missing_copy_implementations)]
// #[resource(item_abilities)]
pub struct ItemAbility {
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
	id: Rowid,
}
/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow,Resource)]
#[subresource(item_effects,itemref,items)]
#[allow(missing_copy_implementations)]
// #[resource(item_effects)]
pub struct ItemEffect {
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
	id: Rowid,
}

pub trait ToplevelResourceData: Resource {
	// TODO:
	// fn resref(&self)->Resref;
	const EXTENSION: &'static str;
	const RESTYPE: Restype;
}
/// A top-level resource (i.e. saved to its own file in the override
/// directory).
pub trait ToplevelResource: ToplevelResourceData {
	/// Subresources for this resource (together with linking info.).
	///
	/// This should be a tuple of all subresources, each of them stored in
	/// some `Vec` with elements (subresource, linking-info).
	type Subresources<'a>;
	/// Inserts a single resource in the database.
	fn load(tables: &mut AllResources<Statement<'_>>, cursor: impl Read+Seek, resref: Resref)
		->Result<()>;
	fn load_from_handle(db: &mut DbInserter<'_>, resref: Resref,
			mut handle: crate::gamefiles::ResHandle<'_>)->Result<()> {
		Self::load(&mut db.tables, handle.open()?, resref)?;
		if handle.is_override() {
			db.add_override.execute((resref, Self::EXTENSION))?;
		}
// // 		*(<T as NamedTable>::find_field_mut(&mut self.resource_count))+=1;
// 		*count+= 1;
		Ok(())
	}
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
	fn for_each<F,C>(db: &impl DbInterface, condition: C, f: F)->Result<i32>
		where F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>,
		C: Display;
	/// Saves all toplevel resources of this type.
	fn save_all(db: &impl DbInterface)->Result<()> {
		let Schema { name, .. } = Self::schema();
		let extension = Self::EXTENSION;
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
	fn load(tables: &mut AllResources<Statement<'_>>, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let mut item = Item::unpack(&mut cursor)
			.context("cannot unpack Item0 main struct")?;
		item.itemref = resref.into();
		tables.items.execute(item.as_params())
			.context("inserting into 'items'")?;
		cursor.seek(SeekFrom::Start(item.abilities_offset.unwrap() as u64))?;

		let mut ab_n = Vec::<u16>::
			with_capacity(item.abilities_count.unwrap() as usize);
		for ab_index in 1..1+item.abilities_count.unwrap() {
			let mut ab = ItemAbility::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack item ability {}/{}",
					ab_index, item.abilities_count))?;
			ab.itemref = resref.into();
			ab.index = ab_index.into();
			tables.item_abilities.execute(ab.as_params())
				.context("inserting into 'item_abilities'")?;
			ab_n.push(ab.effect_count.unwrap());
		}
		trace!("inserting item {resref}; abilities have {ab_n:?} effects");
		cursor.seek(SeekFrom::Start(item.effect_offset.unwrap() as u64))?;
		for j in 1..1+item.equip_effect_count.unwrap() { // on-equip effects
			let mut eff = ItemEffect::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack global item effect {}/{}",
					j+1, item.equip_effect_count.unwrap()))?;
			eff.itemref = resref.into();
			eff.ability = 0.into();
			eff.index = j.into();
			tables.item_effects.execute(eff.as_params())
				.context("inserting into 'item_effects'")?;
		}
		for (i, n) in ab_n.iter().enumerate() {
			for j in 1..(1+*n) {
				let mut eff = ItemEffect::unpack(&mut cursor)
					.with_context(|| format!("cannot unpack item effect {}/{} for ability {}", j+1, n, i+1))?;
				eff.itemref = resref.into();
				eff.ability = (1+i as i64).into();
				eff.index = j.into();
				tables.item_effects.execute(eff.as_params())
					.context("inserting into 'item_effects'")?;
			}
		}
	Ok(())
	}
	/// saves an item (with its sub-resources) to a file
	fn save(&mut self, mut file: impl Write+Seek+Debug, (abilities, effects): Self::Subresources<'_>) ->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			abilities.len(), effects.len());
		let mut current_effect_idx = self.equip_effect_count.unwrap();
		for a in abilities.iter_mut() {
			a.effect_index = current_effect_idx.into();
			current_effect_idx+= a.effect_count.unwrap();
		}
		self.abilities_offset = 114.into();
		self.effect_offset = (114 + 56*(self.abilities_count.unwrap() as u32)).into();
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
		for i in 0..self.abilities_count.unwrap() {
			for (e, j) in effects.iter() {
				if *j == (i+1).into() {
					e.pack(&mut file)?;
				}
			}
		}
		Ok(())
	}
	/// Selects a number of items from the database.
	fn for_each<F,C>(db: &impl DbInterface, condition: C, f:F)->Result<i32>
		where F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>,
			C: Display {
		let mut sel_item = Item::select_typed(db, &condition)?;
		let mut sel_item_ab = ItemAbility::select_typed(db, r#"where "key"=?"#)?;
		let mut sel_item_eff = ItemEffect::select_typed(db, r#"where "key"=?"#)?;
		let mut n_items = 0;
		debug!("processing items under condition: {condition}");
		for x in sel_item.iter(())? {
			if x.is_db_malformed() { continue }
			let mut item = x?;
			debug!("reading item: {}", item.itemref);
			// we store item effects & abilities in two vectors:
			//  - abilities = (ability, abref)
			//  - effect = (effect, ability index)
			let mut abilities = Vec::<ItemAbility>::new();
			let mut effects = Vec::<(ItemEffect, usize)>::new();
			for x in sel_item_ab.iter((&item.itemref,))? {
				if x.is_db_malformed() { continue }
				abilities.push(x?);
				item.abilities_count+= 1;
			}
			for x in sel_item_eff.iter((&item.itemref,))? {
				if x.is_db_malformed() { continue }
				let eff = x?;
				let index = abilities.iter().position(|ab|
				// we might throw an error here — but [`Iterator::position`]
				// does not accept [`Result`] values.
					ab.id.unwrap().expect("null rowid in item abilities")
						== eff.ability.unwrap());
				let ab_id = match index {
					Some(i) => { abilities[i].effect_count+= 1; i+1 },
					None => { item.equip_effect_count+= 1; 0 },
				};
				effects.push((eff, ab_id));
			}
			f(item.itemref.unwrap(), &mut item, (&mut abilities, &mut effects))?;
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
all_resources!();

impl AllResources<Schema> {
	/// Builds the SQL statement creating the `new_strings` view.
	///
	/// This also builds a few triggers which mark resources as dirty
	/// whenever their strrefs are reassigned.
	pub fn create_new_strings<T>(&mut self, f: impl Fn(&str)->Result<T>)->Result<()> {
		let mut create = String::from(
r#"create view "new_strings"("native","strref","flags") as
select "a"."native", "strref", "flags" from ("#);
		let mut is_first = true;
		let mut trigger = String::from(r#"create trigger "update_strrefs"
after insert on "strref_dict"
begin"#);
		self.map_mut(|schema| {
			schema.append_new_strings_schema(&mut create, &mut is_first);
			schema.append_new_strings_trigger(&mut trigger);
			any_ok(())
		})?;
		write!(&mut create, r#") as "a"
	left join "strref_dict" as "b" on "a"."native" = "b"."native"
	where typeof("a"."native") = 'text'"#)?;
		write!(&mut trigger, "end")?;
		f(&create)?;
		f(&trigger)?;
		// This triggers makes any update of `strref` in `new_strings`
		// (with any dummy value; we use -1) produce the lowest still-available
		// strref; see
		// https://stackoverflow.com/questions/24587799/returning-the-lowest-integer-not-in-a-list-in-sql
		f(r#"create trigger "update_new_strings"
	instead of update of "strref" on "new_strings"
	begin
		insert into "strref_dict"("native","strref")
		values (new."native",
			ifnull((select min("strref")+1 from "strref_dict"
				where "strref"+1 not in (select "strref" from "strref_dict")),
				(select "strref_count" from "global")));
	end"#)?;
		Ok(())
	}
}

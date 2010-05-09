/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package collection.generic;

/**
 * Specifies a default value for a type B, for use by DomainMap constructors.
 * Implicits for all primitive and reference types are defined in the companion
 * object.
 *
 * @author dramage
 */
class DefaultValue[@specialized B](val value : B) {
  override def toString = "DefaultValue("+value+")";
}

object DefaultValue {
  implicit val defaultDouble : DefaultValue[Double] =
    new DefaultValue(0.0);

  implicit val defaultFloat : DefaultValue[Float] =
    new DefaultValue(0.0f);

  implicit val defaultBoolean : DefaultValue[Boolean] =
    new DefaultValue(false);

  implicit val defaultInt : DefaultValue[Int] =
    new DefaultValue(0);

  implicit val defaultLong : DefaultValue[Long] =
    new DefaultValue(0l);

  implicit val defaultByte : DefaultValue[Byte] =
    new DefaultValue(0.asInstanceOf[Byte]);

  implicit val defaultShort : DefaultValue[Short] =
    new DefaultValue(0.asInstanceOf[Short]);

  implicit val defaultChar : DefaultValue[Char] =
    new DefaultValue(0.asInstanceOf[Char]);

  protected val defaultRefInstance = new DefaultValue[AnyRef](null);
  implicit def defaultRef[X<:AnyRef] =
    defaultRefInstance.asInstanceOf[DefaultValue[X]];
}

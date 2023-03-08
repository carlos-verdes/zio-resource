/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn

enum ResourceError(msg: String, cause: Option[Throwable] = None) extends Throwable(msg, cause.orNull):
  case NotFoundError(urn: Option[Urn], cause: Option[Throwable])
      extends ResourceError(s"Resource ${withId(urn)} not found", cause)
  case SerializationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case NormalizationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case FormatError(msg: String, cause: Option[Throwable] = None)
      extends ResourceError(s"Format not supported: $msg", cause)
  case UnderlinedError(cause: Throwable) extends ResourceError("Non controlled error", Some(cause))

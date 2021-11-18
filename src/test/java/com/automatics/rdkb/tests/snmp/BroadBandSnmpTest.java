/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.snmp;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.error.ErrorType;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;

public class BroadBandSnmpTest extends AutomaticsTestBase {

	/**
	 * Test to Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2
	 * (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB.
	 * 
	 * @param device The Device to be used.
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1004")

	public void testSnmpGetOnCableModemMacAddress(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-004";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpCableModemMacAddress = null;
		String cableModemMacAddress = null;

		LOGGER.info("#######################################################################################");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2)");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1:Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
		LOGGER.info("#######################################################################################");

		/**
		 * Step 1 : Retrieve the CM Mac Address from the response of
		 * IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the
		 * device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: DESCRIPTION: Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
		LOGGER.info(
				"STEP 1:ACTION: Execute SNMP command to retrive the CM MAC Address by using SNMP oid .1.3.6.1.2.1.2.2.1.6.2");

		LOGGER.info("EXPECTED: Should return the Device Cable modem address");
		LOGGER.info("**********************************************************************************");
		try {

			// Retrieve the Cable Modem Mac Address from the SNMP command
			snmpCableModemMacAddress = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), "2");
			LOGGER.info("SNMP command output for IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) : "
					+ snmpCableModemMacAddress);
			/*
			 * Cable-modem MAC address is retrieved from BHC.
			 */
			cableModemMacAddress = BroadBandSnmpUtils
					.formatMacAddressWithoutLeadingZeros(((Device) device).getEcmMac());

			message = " EXPECTED: Cable Modem MAC Address  : " + cableModemMacAddress;

			if (CommonMethods.isNotNull(snmpCableModemMacAddress) && CommonMethods.isNotNull(cableModemMacAddress)) {
				// Verify the retrieved MAC with the actual MAC from CHIMPS
				status = (snmpCableModemMacAddress.trim()).equalsIgnoreCase(cableModemMacAddress.trim());

				if (!status) {
					message = "Seems like IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) providing wrong Cable Modem Address . ACTUAL :  Cable Modem Address :  "
							+ snmpCableModemMacAddress + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve Cable Modem Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB";
				LOGGER.error(message);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to Verify the Manufacturer serial number using
	 * DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB.
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1002")

	public void testSnmpGetOnDocsisCableDeviceSerialNumber(Dut device) {
		String testCaseId = "TC-RDKB-SNMP-002";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpDocsisDeviceSerialNumber = null;
		String manufacturerSerialNumber = null;

		/**
		 * Step 1 : Retrieve the Serial number from the response of
		 * DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command and verify with the actual
		 * serial number of the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1:DESCRIPTION: Retrieve the Serial number from the response of DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command and verify with the actual serial number of the device using device object. If device object not configured webpa param will be used.");
		LOGGER.info(
				"STEP 1:ACTION: Execute snmp command DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber and cross check the value with the value retrived from CATS/ webpa");
		LOGGER.info("EXPECTED: Serial number should be same for snmp and values retrieved using CATS/webpa");
		LOGGER.info("**********************************************************************************");
		try {

			// Retrieve the serial number from the response
			snmpDocsisDeviceSerialNumber = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_CABLE_DEVICE_MIB_DOCS_DEV_SERIAL_NUMBER.getOid());
			LOGGER.info("SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) : "
					+ snmpDocsisDeviceSerialNumber);
			manufacturerSerialNumber = device.getSerialNumber();
			LOGGER.info("manufacturerSerialNumber obtained from CATS " + manufacturerSerialNumber);
			message = " EXPECTED: Manufacturer Serial Number  : " + manufacturerSerialNumber;

			if (CommonMethods.isNotNull(snmpDocsisDeviceSerialNumber)
					&& CommonMethods.isNotNull(manufacturerSerialNumber)) {
				// Verify with the actual serial number of the device
				status = (snmpDocsisDeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());
				if (!status) {
					message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) providing wrong serial number . ACTUAL : Manufacture Serial number :  "
							+ snmpDocsisDeviceSerialNumber + message;
					LOGGER.error(message);
					status = (snmpDocsisDeviceSerialNumber.trim())
							.equalsIgnoreCase(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
									BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER).trim());
					LOGGER.error(
							"Since device object validation failed, Status of serial number validation done via webapa: "
									+ status);
				}
			} else {
				status = false;
				message = "Unable to retrieve manufacturer serial number using DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB";
				LOGGER.error(message);
			}
			if (status) {
				LOGGER.info("STEP 1 : ACTUAL : Successfully verified serial number");
			} else {
				LOGGER.error("STEP 1 : ACTUAL : " + message);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1
	 * (.1.3.6.1.2.1.2.2.1.6.1) SNMP MIB.
	 * 
	 * 
	 * <ol>
	 * <li>Step 1 : Retrieve the Mac Address from the response of
	 * IF-MIB::ifPhysAddress.1 command and verify with the actual MAC Address of the
	 * device using device object</li>
	 * </ol>
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 * 
	 * @param device {@link Dut}
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1003")
	public void testSnmpGetOnWanMacAddress(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-003";
		String stepNumber = "s1";
		boolean status = false;
		String errorMessage = null;
		String snmpWanMacAddress = null;
		String wanMacAddress = null;
		String ifTableIndex = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1003");
		LOGGER.info(
				"TEST DESCRIPTION:Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) ");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info(
				"1:Verify the Mac Address from the response of IF-MIB::ifPhysAddress.1(for pace xf3 getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
		LOGGER.info("#######################################################################################");

		try {
			errorMessage = "Seems like IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) providing wrong WAN MAC Address.";
			/**
			 * Step 1 : Retrieve the Mac Address from the response of
			 * IF-MIB::ifPhysAddress.1 command and verify with the actual MAC Address of the
			 * device using device object
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1:DESCRIPTION: Retrieve the Mac Address from the response of IF-MIB::ifPhysAddress.1(for pace xf3 getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
			LOGGER.info(
					"STEP 1:ACTION: Execute SNMP command to retrive the WAN MAC Address by using SNMP and compare with Wan Mac address retrived using device object");
			LOGGER.info("STEP 1:EXPECTED: Should return the Device WAN MAC Address");
			LOGGER.info("**********************************************************************************");
			/*
			 * According to XF3-3542 choosing mib index based on erouter0 interface for XF3.
			 */
			ifTableIndex = BroadBandCommonUtils.getIndexForWanMac(tapEnv, device);
			if (CommonMethods.isNotNull(ifTableIndex)) {
				// TODO Need to check whether below code needed or not
				/*
				 * if (SupportedModelHandler.isPaceCFG3(device) ||
				 * SupportedModelHandler.isPaceXF3(device)) { snmpWanMacAddress =
				 * SnmpUtils.snmpGetOnEstb(tapEnv, device,
				 * SnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), ifTableIndex); } else {
				 */
				snmpWanMacAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
						BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), ifTableIndex);
				// }
				LOGGER.info("SNMP command output for IF-MIB::ifPhysAddress." + ifTableIndex
						+ "(.1.3.6.1.2.1.2.2.1.6) : " + snmpWanMacAddress);
				wanMacAddress = BroadBandSnmpUtils.formatMacAddressWithoutLeadingZeros(device.getHostMacAddress());
				if (CommonMethods.isNotNull(snmpWanMacAddress) && CommonMethods.isNotNull(wanMacAddress)) {
					// Verify the retrieved MAC with the actual MAC address of the
					// device
					status = CommonUtils.patternSearchFromTargetString(snmpWanMacAddress, wanMacAddress);
					if (status) {
						LOGGER.info(
								"STEP 1: ACTUAL : retreived Wan MAC address using SNMP and WAN MAC Address using device object are same ");
					} else {
						LOGGER.error("STEP 1: ACTUAL :" + errorMessage + " ACTUAL OUTPUT :  WAN MAC Address :  "
								+ snmpWanMacAddress + " " + " EXPECTED OUTPUT: WAN MAC Address  : " + wanMacAddress);
					}
				} else {
					status = false;
					LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
							+ "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB");
				}
			} else {
				LOGGER.error(
						"Index to get WAN MAC address is null. Hence Will not able to get WAN MAC Address using SNMP.");
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
		} catch (Exception exception) {
			status = false;
			LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
					+ "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB" + exception.getMessage());
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					true);
		}
	}

	/**
	 * Test to Verify the Current software version using
	 * DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) SNMP
	 * MIB.
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @Refactor Athira
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
			BroadBandTestGroup.SNMP_OPERATIONS })
	@TestDetails(testUID = "TC-RDKB-SNMP-1005")
	public void testSnmpGetOnDocsisCableDeviceCurrentSoftwareVersion(Dut device) {

		String testCaseId = "TC-RDKB-SNMP-005";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String snmpDocsisDeviceCurrentSoftwareVersion = null;
		String currentDocsisSwVers = null;
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1005");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
		LOGGER.info(
				"1. Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
		LOGGER.info("#######################################################################################");

		/**
		 * Step 1 : Retrieve the Software version from the response of
		 * DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the
		 * actual SW version of the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
		LOGGER.info("EXPECTED: Should return the current software version");
		LOGGER.info("**********************************************************************************");

		try {
			// Retrieve the software version from the SNMP command -
			// DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers
			snmpDocsisDeviceCurrentSoftwareVersion = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
					BroadBandSnmpMib.ECM_DOCS_DEV_CURRENT_SOFTWARE_VERSION.getOid());
			LOGGER.info(
					"SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) : "
							+ snmpDocsisDeviceCurrentSoftwareVersion);

			currentDocsisSwVers = BroadBandSnmpUtils.getExpectedDocsisCableDeviceCurrentSoftwareVersion(tapEnv, device);

			message = "\t EXPECTED: Currently running software version  : " + currentDocsisSwVers;

			if (CommonMethods.isNotNull(snmpDocsisDeviceCurrentSoftwareVersion)
					&& CommonMethods.isNotNull(currentDocsisSwVers)) {
				// Verify the retrieved version with the actual firmware version
				// of the device
				status = (snmpDocsisDeviceCurrentSoftwareVersion.trim()).equalsIgnoreCase(currentDocsisSwVers.trim());
				if (!status) {
					message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) providing wrong currently running software version. ACTUAL : DOCSIS software version :  "
							+ snmpDocsisDeviceCurrentSoftwareVersion + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve Currently running software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) SNMP MIB";
				LOGGER.error(message);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) details using SNMP MIB"
					+ exception.getMessage();
			LOGGER.error(message);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
		}
	}
}

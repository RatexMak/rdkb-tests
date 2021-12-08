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
package com.automatics.rdkb.tests.multicast;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Test class for IP Multi Cast related functionality.
 * 
 * @author Selvaraj Mariyappan
 * @refacor Govardhan
 */

public class BroadBandIpMultiCastTest extends AutomaticsTestBase{
    /**
     * Verify the CMTS MAC Address to which the gateway is associated can be retrived from SNMP and WebPA
     * <ol>
     * <li>Verify getting the CMTS MAC Address from Arp Table corresponding to erouter0 interface.</li>
     * <li>Verify the CMTS MAC Address can be retrieved via SNMP and cross-verified with the value retrieved from Arp
     * Table.</li>
     * <li>Verify the CMTS MAC Address can be retrieved via WebPA and cross-verified with the value retrieved from Arp
     * Table command.</li>
     * </ol>
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.IP_MULTI_CAST, BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-IP-MCAST-1000")
    public void testBroadBandGatewayProvidesCmtsMacAddress(Dut device) {
	// Variable declaration starts
	String errorMessage = null;
	boolean status = false;
	String testCaseId = "TC-RDKB-IP-MCAST-001";
	String stepNum = "";
	String cmtsMacAddressFromArpCommand = "";
	String formattedMacAddress = "";
	// Variable declaration ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-IP-MCAST-1000");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the CMTS MAC Address to which the gateway is associated can be retrived from SNMP and WebPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify getting the CMTS MAC Address from Arp Table corresponding to erouter0 interface.");
	LOGGER.info(
		"2. Verify the CMTS MAC Address can be retrieved via SNMP and cross-verified with the value retrieved from Arp Table.");
	LOGGER.info(
		"3. Verify the CMTS MAC Address can be retrieved via WebPA and cross-verified with the value retrieved from Arp Table command.");
	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "s1";
	    status = false;
	    errorMessage = "Unable to retrieve CMTS MAC Address from Arp Table corresponding to erouter0 interface.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify getting the CMTS MAC Address from Arp Table corresponding to erouter0 interface.");
	    LOGGER.info(
		    "STEP 1: ACTION : SSH the device and Execute the following command: '/sbin/arp -n | grep -i \"erouter0\"' and Retrieve the MAC Address of CMTS.");
	    LOGGER.info(
		    "STEP 1: EXPECTED : MAC Address of CMTS to which the gateway is associated should be retrieved successfully.");
	    LOGGER.info("**********************************************************************************");
	    try {
		cmtsMacAddressFromArpCommand = BroadBandCommonUtils
			.getCmtsMacAddressFromArpTableErouterInterface(device, tapEnv);
		LOGGER.info("CMTS MAC Address Retieved from ARP Table : " + cmtsMacAddressFromArpCommand);
		status = CommonMethods.isNotNull(cmtsMacAddressFromArpCommand);
	    } catch (Exception e) {
		errorMessage = errorMessage
			+ "Exception occured while getting CMTS MAC Adress from Arp Table corresponding to erouter0 interface."
			+ e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : MAC Address of CMTS to which the gateway is associated is retrieved successfully from Arp Table.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    status = false;
	    errorMessage = "Unable to cross verify the CMTS MAC Address retrieved via SNMP with MAC address retrieved from Arp Table.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify the CMTS MAC Address can be retrieved via SNMP and cross-verified with the value retrieved from Arp Table.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the SNMP Get Command for following OID: 1.3.6.1.2.1.10.127.1.2.1.1.1.2 and cross verify with value from Arp Table.");
	    LOGGER.info("STEP 2: EXPECTED : MAC Address of CMTS retrieved via SNMP and from Arp Table should be same.");
	    LOGGER.info("**********************************************************************************");
	    try {
		String snmpDocsisCmtsMacAddress = BroadBandSnmpUtils.getCmtsMacAddressUsingSnmpCommand(device, tapEnv);
		LOGGER.info("CMTS MAC Address Retieved using SNMP command  = " + snmpDocsisCmtsMacAddress);
		formattedMacAddress = cmtsMacAddressFromArpCommand
			.replace(BroadBandTestConstants.DELIMITER_COLON, BroadBandTestConstants.SINGLE_SPACE_CHARACTER)
			.toUpperCase();
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			formattedMacAddress, snmpDocsisCmtsMacAddress);
	    } catch (Exception e) {
		errorMessage = errorMessage
			+ "Exception occured while getting CMTS MAC Adress via SNMP and cross verifying with Arp Table value."
			+ e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : MAC Address of CMTS to which the gateway is associated is retrieved successfully using SNMP and cross-verified with the value retrieved from Arp Table command.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s3";
	    status = false;
	    errorMessage = "Unable to cross verify the CMTS MAC Address retrieved via WebPA with MAC address retrieved from Arp Table command.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the CMTS MAC Address can be retrieved via WebPA and cross-verified with the value retrieved from Arp Table command.");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the WebPA Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_CMTS_MAC and cross verify with value from Arp Table.");
	    LOGGER.info(
		    "STEP 3: EXPECTED : MAC Address of CMTS retrieved via WebPA and value from Arp Table should be same.");
	    LOGGER.info("**********************************************************************************");
	    try {
		String webPaCmtsMacAddress = BroadBandWebPaUtils.getCmtsMacAddressUsingWebPaCommand(device, tapEnv);
		LOGGER.info("CMTS MAC Address Retieved using WebPA command  = " + webPaCmtsMacAddress);
		status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			cmtsMacAddressFromArpCommand, webPaCmtsMacAddress);
	    } catch (Exception e) {
		errorMessage = errorMessage
			+ "Exception occured while getting CMTS MAC Adress via WebPa and cross verifying with Arp Table value."
			+ e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : MAC Address of CMTS to which the gateway is associated is retrieved successfully using WebPA and cross-verified with the value retrieved from Arp Table.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-IP-MCAST-1000");
    }

}
